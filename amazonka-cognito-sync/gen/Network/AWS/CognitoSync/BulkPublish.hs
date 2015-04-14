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

-- Module      : Network.AWS.CognitoSync.BulkPublish
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

-- | Initiates a bulk publish of all existing datasets for an Identity Pool to the
-- configured stream. Customers are limited to one successful bulk publish per
-- 24 hours. Bulk publish is an asynchronous request, customers can see the
-- status of the request via the GetBulkPublishDetails operation.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_BulkPublish.html>
module Network.AWS.CognitoSync.BulkPublish
    (
    -- * Request
      BulkPublish
    -- ** Request constructor
    , bulkPublish
    -- ** Request lenses
    , bpIdentityPoolId

    -- * Response
    , BulkPublishResponse
    -- ** Response constructor
    , bulkPublishResponse
    -- ** Response lenses
    , bprIdentityPoolId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

newtype BulkPublish = BulkPublish
    { _bpIdentityPoolId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'BulkPublish' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bpIdentityPoolId' @::@ 'Text'
--
bulkPublish :: Text -- ^ 'bpIdentityPoolId'
            -> BulkPublish
bulkPublish p1 = BulkPublish
    { _bpIdentityPoolId = p1
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
bpIdentityPoolId :: Lens' BulkPublish Text
bpIdentityPoolId = lens _bpIdentityPoolId (\s a -> s { _bpIdentityPoolId = a })

newtype BulkPublishResponse = BulkPublishResponse
    { _bprIdentityPoolId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'BulkPublishResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bprIdentityPoolId' @::@ 'Maybe' 'Text'
--
bulkPublishResponse :: BulkPublishResponse
bulkPublishResponse = BulkPublishResponse
    { _bprIdentityPoolId = Nothing
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
bprIdentityPoolId :: Lens' BulkPublishResponse (Maybe Text)
bprIdentityPoolId =
    lens _bprIdentityPoolId (\s a -> s { _bprIdentityPoolId = a })

instance ToPath BulkPublish where
    toPath BulkPublish{..} = mconcat
        [ "/identitypools/"
        , toText _bpIdentityPoolId
        , "/bulkpublish"
        ]

instance ToQuery BulkPublish where
    toQuery = const mempty

instance ToHeaders BulkPublish

instance ToJSON BulkPublish where
    toJSON = const (toJSON Empty)

instance AWSRequest BulkPublish where
    type Sv BulkPublish = CognitoSync
    type Rs BulkPublish = BulkPublishResponse

    request  = post
    response = jsonResponse

instance FromJSON BulkPublishResponse where
    parseJSON = withObject "BulkPublishResponse" $ \o -> BulkPublishResponse
        <$> o .:? "IdentityPoolId"
