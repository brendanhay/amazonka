{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the compiled information from a RequestEnvironmentInfo request.
-- Related Topics RequestEnvironmentInfo
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &InfoType=tail
-- &Operation=RetrieveEnvironmentInfo &AuthParams
-- https://elasticbeanstalk.us-east-1.s3.amazonaws.com/environments%2Fa514386a-709f-4888-9683-068c38d744b4%2Flogs%2Fi-92a3ceff%2F278756a8-7d83-4bc1-93db-b1763163705a.log?Expires=1291236023
-- &AuthParams 2010-11-17T20:40:23.210Z tail i-92a3ceff
-- e8e785c9-f28a-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      RetrieveEnvironmentInfo
    -- ** Request constructor
    , mkRetrieveEnvironmentInfo
    -- ** Request lenses
    , rei1EnvironmentId
    , rei1EnvironmentName
    , rei1InfoType

    -- * Response
    , RetrieveEnvironmentInfoResponse
    -- ** Response constructor
    , mkRetrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirEnvironmentInfo
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo
    { _rei1EnvironmentId :: Maybe Text
    , _rei1EnvironmentName :: Maybe Text
    , _rei1InfoType :: EnvironmentInfoType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveEnvironmentInfo' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @InfoType ::@ @EnvironmentInfoType@
--
mkRetrieveEnvironmentInfo :: EnvironmentInfoType -- ^ 'rei1InfoType'
                          -> RetrieveEnvironmentInfo
mkRetrieveEnvironmentInfo p3 = RetrieveEnvironmentInfo
    { _rei1EnvironmentId = Nothing
    , _rei1EnvironmentName = Nothing
    , _rei1InfoType = p3
    }

-- | The ID of the data's environment. If no such environment is found, returns
-- an InvalidParameterValue error. Condition: You must specify either this or
-- an EnvironmentName, or both. If you do not specify either, AWS Elastic
-- Beanstalk returns MissingRequiredParameter error.
rei1EnvironmentId :: Lens' RetrieveEnvironmentInfo (Maybe Text)
rei1EnvironmentId =
    lens _rei1EnvironmentId (\s a -> s { _rei1EnvironmentId = a })

-- | The name of the data's environment. If no such environment is found,
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
rei1EnvironmentName :: Lens' RetrieveEnvironmentInfo (Maybe Text)
rei1EnvironmentName =
    lens _rei1EnvironmentName (\s a -> s { _rei1EnvironmentName = a })

-- | The type of information to retrieve.
rei1InfoType :: Lens' RetrieveEnvironmentInfo EnvironmentInfoType
rei1InfoType = lens _rei1InfoType (\s a -> s { _rei1InfoType = a })

instance ToQuery RetrieveEnvironmentInfo where
    toQuery = genericQuery def

-- | Result message containing a description of the requested environment info.
newtype RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { _reirEnvironmentInfo :: [EnvironmentInfoDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveEnvironmentInfoResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentInfo ::@ @[EnvironmentInfoDescription]@
--
mkRetrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse
mkRetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { _reirEnvironmentInfo = mempty
    }

-- | The EnvironmentInfoDescription of the environment.
reirEnvironmentInfo :: Lens' RetrieveEnvironmentInfoResponse [EnvironmentInfoDescription]
reirEnvironmentInfo =
    lens _reirEnvironmentInfo (\s a -> s { _reirEnvironmentInfo = a })

instance FromXML RetrieveEnvironmentInfoResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RetrieveEnvironmentInfo where
    type Sv RetrieveEnvironmentInfo = ElasticBeanstalk
    type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse

    request = post "RetrieveEnvironmentInfo"
    response _ = xmlResponse
