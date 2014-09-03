{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo
    (
    -- * Request
      RetrieveEnvironmentInfo
    -- ** Request constructor
    , retrieveEnvironmentInfo
    -- ** Request lenses
    , reinInfoType
    , reinEnvironmentId
    , reinEnvironmentName

    -- * Response
    , RetrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirmEnvironmentInfo
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RetrieveEnvironmentInfo' request.
retrieveEnvironmentInfo :: EnvironmentInfoType -- ^ 'reinInfoType'
                        -> RetrieveEnvironmentInfo
retrieveEnvironmentInfo p1 = RetrieveEnvironmentInfo
    { _reinInfoType = p1
    , _reinEnvironmentId = Nothing
    , _reinEnvironmentName = Nothing
    }

data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo
    { _reinInfoType :: EnvironmentInfoType
      -- ^ The type of information to retrieve.
    , _reinEnvironmentId :: Maybe Text
      -- ^ The ID of the data's environment. If no such environment is
      -- found, returns an InvalidParameterValue error. Condition: You
      -- must specify either this or an EnvironmentName, or both. If you
      -- do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _reinEnvironmentName :: Maybe Text
      -- ^ The name of the data's environment. If no such environment is
      -- found, returns an InvalidParameterValue error. Condition: You
      -- must specify either this or an EnvironmentId, or both. If you do
      -- not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    } deriving (Show, Generic)

-- | The type of information to retrieve.
reinInfoType
    :: Functor f
    => (EnvironmentInfoType
    -> f (EnvironmentInfoType))
    -> RetrieveEnvironmentInfo
    -> f RetrieveEnvironmentInfo
reinInfoType f x =
    (\y -> x { _reinInfoType = y })
       <$> f (_reinInfoType x)
{-# INLINE reinInfoType #-}

-- | The ID of the data's environment. If no such environment is found, returns
-- an InvalidParameterValue error. Condition: You must specify either this or
-- an EnvironmentName, or both. If you do not specify either, AWS Elastic
-- Beanstalk returns MissingRequiredParameter error.
reinEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RetrieveEnvironmentInfo
    -> f RetrieveEnvironmentInfo
reinEnvironmentId f x =
    (\y -> x { _reinEnvironmentId = y })
       <$> f (_reinEnvironmentId x)
{-# INLINE reinEnvironmentId #-}

-- | The name of the data's environment. If no such environment is found,
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
reinEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RetrieveEnvironmentInfo
    -> f RetrieveEnvironmentInfo
reinEnvironmentName f x =
    (\y -> x { _reinEnvironmentName = y })
       <$> f (_reinEnvironmentName x)
{-# INLINE reinEnvironmentName #-}

instance ToQuery RetrieveEnvironmentInfo where
    toQuery = genericQuery def

data RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { _reirmEnvironmentInfo :: [EnvironmentInfoDescription]
      -- ^ The EnvironmentInfoDescription of the environment.
    } deriving (Show, Generic)

-- | The EnvironmentInfoDescription of the environment.
reirmEnvironmentInfo
    :: Functor f
    => ([EnvironmentInfoDescription]
    -> f ([EnvironmentInfoDescription]))
    -> RetrieveEnvironmentInfoResponse
    -> f RetrieveEnvironmentInfoResponse
reirmEnvironmentInfo f x =
    (\y -> x { _reirmEnvironmentInfo = y })
       <$> f (_reirmEnvironmentInfo x)
{-# INLINE reirmEnvironmentInfo #-}

instance FromXML RetrieveEnvironmentInfoResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RetrieveEnvironmentInfo where
    type Sv RetrieveEnvironmentInfo = ElasticBeanstalk
    type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse

    request = post "RetrieveEnvironmentInfo"
    response _ = xmlResponse
