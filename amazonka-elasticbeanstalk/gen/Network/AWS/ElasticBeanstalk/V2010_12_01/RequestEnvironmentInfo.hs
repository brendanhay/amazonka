{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo
    (
    -- * Request
      RequestEnvironmentInfo
    -- ** Request constructor
    , mkRequestEnvironmentInfoMessage
    -- ** Request lenses
    , reimEnvironmentId
    , reimEnvironmentName
    , reimInfoType

    -- * Response
    , RequestEnvironmentInfoResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RequestEnvironmentInfo' request.
mkRequestEnvironmentInfoMessage :: EnvironmentInfoType -- ^ 'reimInfoType'
                                -> RequestEnvironmentInfo
mkRequestEnvironmentInfoMessage p1 = RequestEnvironmentInfo
    { _reimEnvironmentId = Nothing
    , _reimEnvironmentName = Nothing
    , _reimInfoType = p3
    }
{-# INLINE mkRequestEnvironmentInfoMessage #-}

data RequestEnvironmentInfo = RequestEnvironmentInfo
    { _reimEnvironmentId :: Maybe Text
      -- ^ The ID of the environment of the requested data. If no such
      -- environment is found, RequestEnvironmentInfo returns an
      -- InvalidParameterValue error. Condition: You must specify either
      -- this or an EnvironmentName, or both. If you do not specify
      -- either, AWS Elastic Beanstalk returns MissingRequiredParameter
      -- error.
    , _reimEnvironmentName :: Maybe Text
      -- ^ The name of the environment of the requested data. If no such
      -- environment is found, RequestEnvironmentInfo returns an
      -- InvalidParameterValue error. Condition: You must specify either
      -- this or an EnvironmentId, or both. If you do not specify either,
      -- AWS Elastic Beanstalk returns MissingRequiredParameter error.
    , _reimInfoType :: EnvironmentInfoType
      -- ^ The type of information to request.
    } deriving (Show, Generic)

-- | The ID of the environment of the requested data. If no such environment is
-- found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentName, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reimEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reimEnvironmentId = lens _reimEnvironmentId (\s a -> s { _reimEnvironmentId = a })
{-# INLINE reimEnvironmentId #-}

-- | The name of the environment of the requested data. If no such environment
-- is found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reimEnvironmentName :: Lens' RequestEnvironmentInfo (Maybe Text)
reimEnvironmentName = lens _reimEnvironmentName (\s a -> s { _reimEnvironmentName = a })
{-# INLINE reimEnvironmentName #-}

-- | The type of information to request.
reimInfoType :: Lens' RequestEnvironmentInfo (EnvironmentInfoType)
reimInfoType = lens _reimInfoType (\s a -> s { _reimInfoType = a })
{-# INLINE reimInfoType #-}

instance ToQuery RequestEnvironmentInfo where
    toQuery = genericQuery def

data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RequestEnvironmentInfo where
    type Sv RequestEnvironmentInfo = ElasticBeanstalk
    type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse

    request = post "RequestEnvironmentInfo"
    response _ = nullaryResponse RequestEnvironmentInfoResponse
