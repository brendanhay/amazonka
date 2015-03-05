{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Get the status of the last BulkPublish operation for an identity pool.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetBulkPublishDetails.html>
module Network.AWS.CognitoSync.GetBulkPublishDetails
    (
    -- * Request
      GetBulkPublishDetails
    -- ** Request constructor
    , getBulkPublishDetails
    -- ** Request lenses
    , gbpdIdentityPoolId

    -- * Response
    , GetBulkPublishDetailsResponse
    -- ** Response constructor
    , getBulkPublishDetailsResponse
    -- ** Response lenses
    , gbpdrBulkPublishCompleteTime
    , gbpdrBulkPublishStartTime
    , gbpdrBulkPublishStatus
    , gbpdrFailureMessage
    , gbpdrIdentityPoolId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

newtype GetBulkPublishDetails = GetBulkPublishDetails
    { _gbpdIdentityPoolId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetBulkPublishDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpdIdentityPoolId' @::@ 'Text'
--
getBulkPublishDetails :: Text -- ^ 'gbpdIdentityPoolId'
                      -> GetBulkPublishDetails
getBulkPublishDetails p1 = GetBulkPublishDetails
    { _gbpdIdentityPoolId = p1
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
gbpdIdentityPoolId :: Lens' GetBulkPublishDetails Text
gbpdIdentityPoolId =
    lens _gbpdIdentityPoolId (\s a -> s { _gbpdIdentityPoolId = a })

data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse
    { _gbpdrBulkPublishCompleteTime :: Maybe POSIX
    , _gbpdrBulkPublishStartTime    :: Maybe POSIX
    , _gbpdrBulkPublishStatus       :: Maybe BulkPublishStatus
    , _gbpdrFailureMessage          :: Maybe Text
    , _gbpdrIdentityPoolId          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GetBulkPublishDetailsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpdrBulkPublishCompleteTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gbpdrBulkPublishStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gbpdrBulkPublishStatus' @::@ 'Maybe' 'BulkPublishStatus'
--
-- * 'gbpdrFailureMessage' @::@ 'Maybe' 'Text'
--
-- * 'gbpdrIdentityPoolId' @::@ 'Maybe' 'Text'
--
getBulkPublishDetailsResponse :: GetBulkPublishDetailsResponse
getBulkPublishDetailsResponse = GetBulkPublishDetailsResponse
    { _gbpdrIdentityPoolId          = Nothing
    , _gbpdrBulkPublishStartTime    = Nothing
    , _gbpdrBulkPublishCompleteTime = Nothing
    , _gbpdrBulkPublishStatus       = Nothing
    , _gbpdrFailureMessage          = Nothing
    }

-- | If 'BulkPublishStatus' is SUCCEEDED, the time the last bulk publish operation
-- completed.
gbpdrBulkPublishCompleteTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrBulkPublishCompleteTime =
    lens _gbpdrBulkPublishCompleteTime
        (\s a -> s { _gbpdrBulkPublishCompleteTime = a })
            . mapping _Time

-- | The date/time at which the last bulk publish was initiated.
gbpdrBulkPublishStartTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrBulkPublishStartTime =
    lens _gbpdrBulkPublishStartTime
        (\s a -> s { _gbpdrBulkPublishStartTime = a })
            . mapping _Time

-- | Status of the last bulk publish operation, valid values are: 'NOT_STARTED' - No
-- bulk publish has been requested for this identity pool
--
-- 'IN_PROGRESS' - Data is being published to the configured stream
--
-- 'SUCCEEDED' - All data for the identity pool has been published to the
-- configured stream
--
-- 'FAILED' - Some portion of the data has failed to publish, check 'FailureMessage'
-- for the cause.
gbpdrBulkPublishStatus :: Lens' GetBulkPublishDetailsResponse (Maybe BulkPublishStatus)
gbpdrBulkPublishStatus =
    lens _gbpdrBulkPublishStatus (\s a -> s { _gbpdrBulkPublishStatus = a })

-- | If BulkPublishStatus is FAILED this field will contain the error message that
-- caused the bulk publish to fail.
gbpdrFailureMessage :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrFailureMessage =
    lens _gbpdrFailureMessage (\s a -> s { _gbpdrFailureMessage = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
gbpdrIdentityPoolId :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrIdentityPoolId =
    lens _gbpdrIdentityPoolId (\s a -> s { _gbpdrIdentityPoolId = a })

instance ToPath GetBulkPublishDetails where
    toPath GetBulkPublishDetails{..} = mconcat
        [ "/identitypools/"
        , toText _gbpdIdentityPoolId
        , "/getBulkPublishDetails"
        ]

instance ToQuery GetBulkPublishDetails where
    toQuery = const mempty

instance ToHeaders GetBulkPublishDetails

instance ToJSON GetBulkPublishDetails where
    toJSON = const (toJSON Empty)

instance AWSRequest GetBulkPublishDetails where
    type Sv GetBulkPublishDetails = CognitoSync
    type Rs GetBulkPublishDetails = GetBulkPublishDetailsResponse

    request  = post
    response = jsonResponse

instance FromJSON GetBulkPublishDetailsResponse where
    parseJSON = withObject "GetBulkPublishDetailsResponse" $ \o -> GetBulkPublishDetailsResponse
        <$> o .:? "BulkPublishCompleteTime"
        <*> o .:? "BulkPublishStartTime"
        <*> o .:? "BulkPublishStatus"
        <*> o .:? "FailureMessage"
        <*> o .:? "IdentityPoolId"
