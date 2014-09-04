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
    , requestEnvironmentInfo
    -- ** Request lenses
    , reimInfoType
    , reimEnvironmentId
    , reimEnvironmentName

    -- * Response
    , RequestEnvironmentInfoResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RequestEnvironmentInfo' request.
requestEnvironmentInfo :: EnvironmentInfoType -- ^ 'reimInfoType'
                       -> RequestEnvironmentInfo
requestEnvironmentInfo p1 = RequestEnvironmentInfo
    { _reimInfoType = p1
    , _reimEnvironmentId = Nothing
    , _reimEnvironmentName = Nothing
    }
{-# INLINE requestEnvironmentInfo #-}

data RequestEnvironmentInfo = RequestEnvironmentInfo
    { _reimInfoType :: EnvironmentInfoType
      -- ^ The type of information to request.
    , _reimEnvironmentId :: Maybe Text
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
    } deriving (Show, Generic)

-- | The type of information to request.
reimInfoType :: Lens' RequestEnvironmentInfo (EnvironmentInfoType)
reimInfoType f x =
    f (_reimInfoType x)
        <&> \y -> x { _reimInfoType = y }
{-# INLINE reimInfoType #-}

-- | The ID of the environment of the requested data. If no such environment is
-- found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentName, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reimEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reimEnvironmentId f x =
    f (_reimEnvironmentId x)
        <&> \y -> x { _reimEnvironmentId = y }
{-# INLINE reimEnvironmentId #-}

-- | The name of the environment of the requested data. If no such environment
-- is found, RequestEnvironmentInfo returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
reimEnvironmentName :: Lens' RequestEnvironmentInfo (Maybe Text)
reimEnvironmentName f x =
    f (_reimEnvironmentName x)
        <&> \y -> x { _reimEnvironmentName = y }
{-# INLINE reimEnvironmentName #-}

instance ToQuery RequestEnvironmentInfo where
    toQuery = genericQuery def

data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RequestEnvironmentInfo where
    type Sv RequestEnvironmentInfo = ElasticBeanstalk
    type Rs RequestEnvironmentInfo = RequestEnvironmentInfoResponse

    request = post "RequestEnvironmentInfo"
    response _ = nullaryResponse RequestEnvironmentInfoResponse
