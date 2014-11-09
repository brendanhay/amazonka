{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the compiled information from a RequestEnvironmentInfo request.
-- Related Topics RequestEnvironmentInfo.
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
    (
    -- * Request
      RetrieveEnvironmentInfoMessage
    -- ** Request constructor
    , retrieveEnvironmentInfoMessage
    -- ** Request lenses
    , reimEnvironmentId
    , reimEnvironmentName
    , reimInfoType

    -- * Response
    , RetrieveEnvironmentInfoResultMessage
    -- ** Response constructor
    , retrieveEnvironmentInfoResultMessage
    -- ** Response lenses
    , reirmEnvironmentInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data RetrieveEnvironmentInfoMessage = RetrieveEnvironmentInfoMessage
    { _reimEnvironmentId   :: Maybe Text
    , _reimEnvironmentName :: Maybe Text
    , _reimInfoType        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RetrieveEnvironmentInfoMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reimEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'reimEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'reimInfoType' @::@ 'Text'
--
retrieveEnvironmentInfoMessage :: Text -- ^ 'reimInfoType'
                               -> RetrieveEnvironmentInfoMessage
retrieveEnvironmentInfoMessage p1 = RetrieveEnvironmentInfoMessage
    { _reimInfoType        = p1
    , _reimEnvironmentId   = Nothing
    , _reimEnvironmentName = Nothing
    }

-- | The ID of the data's environment. If no such environment is found,
-- returns an InvalidParameterValue error. Condition: You must specify
-- either this or an EnvironmentName, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
reimEnvironmentId :: Lens' RetrieveEnvironmentInfoMessage (Maybe Text)
reimEnvironmentId =
    lens _reimEnvironmentId (\s a -> s { _reimEnvironmentId = a })

-- | The name of the data's environment. If no such environment is found,
-- returns an InvalidParameterValue error. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
reimEnvironmentName :: Lens' RetrieveEnvironmentInfoMessage (Maybe Text)
reimEnvironmentName =
    lens _reimEnvironmentName (\s a -> s { _reimEnvironmentName = a })

-- | The type of information to retrieve.
reimInfoType :: Lens' RetrieveEnvironmentInfoMessage Text
reimInfoType = lens _reimInfoType (\s a -> s { _reimInfoType = a })

instance ToPath RetrieveEnvironmentInfoMessage where
    toPath = const "/"

instance ToQuery RetrieveEnvironmentInfoMessage

newtype RetrieveEnvironmentInfoResultMessage = RetrieveEnvironmentInfoResultMessage
    { _reirmEnvironmentInfo :: [EnvironmentInfoDescription]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'RetrieveEnvironmentInfoResultMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirmEnvironmentInfo' @::@ ['EnvironmentInfoDescription']
--
retrieveEnvironmentInfoResultMessage :: RetrieveEnvironmentInfoResultMessage
retrieveEnvironmentInfoResultMessage = RetrieveEnvironmentInfoResultMessage
    { _reirmEnvironmentInfo = mempty
    }

-- | The EnvironmentInfoDescription of the environment.
reirmEnvironmentInfo :: Lens' RetrieveEnvironmentInfoResultMessage [EnvironmentInfoDescription]
reirmEnvironmentInfo =
    lens _reirmEnvironmentInfo (\s a -> s { _reirmEnvironmentInfo = a })

instance AWSRequest RetrieveEnvironmentInfoMessage where
    type Sv RetrieveEnvironmentInfoMessage = ElasticBeanstalk
    type Rs RetrieveEnvironmentInfoMessage = RetrieveEnvironmentInfoResultMessage

    request  = post "RetrieveEnvironmentInfo"
    response = const . xmlResponse $ \h x -> RetrieveEnvironmentInfoResultMessage
newtype
