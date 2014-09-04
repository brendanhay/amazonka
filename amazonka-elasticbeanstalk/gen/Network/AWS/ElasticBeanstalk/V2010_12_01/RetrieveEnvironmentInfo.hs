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
    , mkRetrieveEnvironmentInfoMessage
    -- ** Request lenses
    , reinEnvironmentId
    , reinEnvironmentName
    , reinInfoType

    -- * Response
    , RetrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirmEnvironmentInfo
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RetrieveEnvironmentInfo' request.
mkRetrieveEnvironmentInfoMessage :: EnvironmentInfoType -- ^ 'reinInfoType'
                                 -> RetrieveEnvironmentInfo
mkRetrieveEnvironmentInfoMessage p1 = RetrieveEnvironmentInfo
    { _reinEnvironmentId = Nothing
    , _reinEnvironmentName = Nothing
    , _reinInfoType = p3
    }
{-# INLINE mkRetrieveEnvironmentInfoMessage #-}

data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo
    { _reinEnvironmentId :: Maybe Text
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
    , _reinInfoType :: EnvironmentInfoType
      -- ^ The type of information to retrieve.
    } deriving (Show, Generic)

-- | The ID of the data's environment. If no such environment is found, returns
-- an InvalidParameterValue error. Condition: You must specify either this or
-- an EnvironmentName, or both. If you do not specify either, AWS Elastic
-- Beanstalk returns MissingRequiredParameter error.
reinEnvironmentId :: Lens' RetrieveEnvironmentInfo (Maybe Text)
reinEnvironmentId = lens _reinEnvironmentId (\s a -> s { _reinEnvironmentId = a })
{-# INLINE reinEnvironmentId #-}

-- | The name of the data's environment. If no such environment is found,
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
reinEnvironmentName :: Lens' RetrieveEnvironmentInfo (Maybe Text)
reinEnvironmentName = lens _reinEnvironmentName (\s a -> s { _reinEnvironmentName = a })
{-# INLINE reinEnvironmentName #-}

-- | The type of information to retrieve.
reinInfoType :: Lens' RetrieveEnvironmentInfo (EnvironmentInfoType)
reinInfoType = lens _reinInfoType (\s a -> s { _reinInfoType = a })
{-# INLINE reinInfoType #-}

instance ToQuery RetrieveEnvironmentInfo where
    toQuery = genericQuery def

newtype RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { _reirmEnvironmentInfo :: [EnvironmentInfoDescription]
      -- ^ The EnvironmentInfoDescription of the environment.
    } deriving (Show, Generic)

-- | The EnvironmentInfoDescription of the environment.
reirmEnvironmentInfo :: Lens' RetrieveEnvironmentInfoResponse ([EnvironmentInfoDescription])
reirmEnvironmentInfo = lens _reirmEnvironmentInfo (\s a -> s { _reirmEnvironmentInfo = a })
{-# INLINE reirmEnvironmentInfo #-}

instance FromXML RetrieveEnvironmentInfoResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RetrieveEnvironmentInfo where
    type Sv RetrieveEnvironmentInfo = ElasticBeanstalk
    type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse

    request = post "RetrieveEnvironmentInfo"
    response _ = xmlResponse
