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

-- | Initiates a request to compile the specified type of information of the
-- deployed environment. Setting the InfoType to tail compiles the last lines
-- from the application server log files of every Amazon EC2 instance in your
-- environment. Use RetrieveEnvironmentInfo to access the compiled
-- information. Related Topics RetrieveEnvironmentInfo
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &InfoType=tail
-- &Operation=RequestEnvironmentInfo &AuthParams
-- 126a4ff3-f28a-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      RequestEnvironmentInfo
    -- ** Request constructor
    , mkRequestEnvironmentInfo
    -- ** Request lenses
    , reiEnvironmentId
    , reiEnvironmentName
    , reiInfoType

    -- * Response
    , RequestEnvironmentInfoResponse
    -- ** Response constructor
    , mkRequestEnvironmentInfoResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data RequestEnvironmentInfo = RequestEnvironmentInfo
    { _reiEnvironmentId :: Maybe Text
    , _reiEnvironmentName :: Maybe Text
    , _reiInfoType :: EnvironmentInfoType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestEnvironmentInfo' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @InfoType ::@ @EnvironmentInfoType@
--
mkRequestEnvironmentInfo :: EnvironmentInfoType -- ^ 'reiInfoType'
                         -> RequestEnvironmentInfo
mkRequestEnvironmentInfo p3 = RequestEnvironmentInfo
    { _reiEnvironmentId = Nothing
    , _reiEnvironmentName = Nothing
    , _reiInfoType = p3
    }

-- | The ID of the environment of the requested data. If no such environment is
-- found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentName, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reiEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentId =
    lens _reiEnvironmentId (\s a -> s { _reiEnvironmentId = a })

-- | The name of the environment of the requested data. If no such environment
-- is found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reiEnvironmentName :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentName =
    lens _reiEnvironmentName (\s a -> s { _reiEnvironmentName = a })

-- | The type of information to request.
reiInfoType :: Lens' RequestEnvironmentInfo EnvironmentInfoType
reiInfoType = lens _reiInfoType (\s a -> s { _reiInfoType = a })

instance ToQuery RequestEnvironmentInfo where
    toQuery = genericQuery def

data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestEnvironmentInfoResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRequestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse
mkRequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse

instance AWSRequest RequestEnvironmentInfo where
    type Sv RequestEnvironmentInfo = ElasticBeanstalk
    type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse

    request = post "RequestEnvironmentInfo"
    response _ = nullaryResponse RequestEnvironmentInfoResponse
