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

-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Swaps the CNAMEs of two environments.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_SwapEnvironmentCNAMEs.html>
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    (
    -- * Request
      SwapEnvironmentCNAMEs
    -- ** Request constructor
    , swapEnvironmentCNAMEs
    -- ** Request lenses
    , secnameDestinationEnvironmentId
    , secnameDestinationEnvironmentName
    , secnameSourceEnvironmentId
    , secnameSourceEnvironmentName

    -- * Response
    , SwapEnvironmentCNAMEsResponse
    -- ** Response constructor
    , swapEnvironmentCNAMEsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs
    { _secnameDestinationEnvironmentId   :: Maybe Text
    , _secnameDestinationEnvironmentName :: Maybe Text
    , _secnameSourceEnvironmentId        :: Maybe Text
    , _secnameSourceEnvironmentName      :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'SwapEnvironmentCNAMEs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'secnameDestinationEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'secnameDestinationEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'secnameSourceEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'secnameSourceEnvironmentName' @::@ 'Maybe' 'Text'
--
swapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs
swapEnvironmentCNAMEs = SwapEnvironmentCNAMEs
    { _secnameSourceEnvironmentId        = Nothing
    , _secnameSourceEnvironmentName      = Nothing
    , _secnameDestinationEnvironmentId   = Nothing
    , _secnameDestinationEnvironmentName = Nothing
    }

-- | The ID of the destination environment. Condition: You must specify at
-- least the 'DestinationEnvironmentID' or the 'DestinationEnvironmentName'.
-- You may also specify both. You must specify the 'SourceEnvironmentId'
-- with the 'DestinationEnvironmentId'.
secnameDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentId =
    lens _secnameDestinationEnvironmentId
        (\s a -> s { _secnameDestinationEnvironmentId = a })

-- | The name of the destination environment. Condition: You must specify at
-- least the 'DestinationEnvironmentID' or the 'DestinationEnvironmentName'.
-- You may also specify both. You must specify the 'SourceEnvironmentName'
-- with the 'DestinationEnvironmentName'.
secnameDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentName =
    lens _secnameDestinationEnvironmentName
        (\s a -> s { _secnameDestinationEnvironmentName = a })

-- | The ID of the source environment. Condition: You must specify at least
-- the 'SourceEnvironmentID' or the 'SourceEnvironmentName'. You may also
-- specify both. If you specify the 'SourceEnvironmentId', you must specify
-- the 'DestinationEnvironmentId'.
secnameSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentId =
    lens _secnameSourceEnvironmentId
        (\s a -> s { _secnameSourceEnvironmentId = a })

-- | The name of the source environment. Condition: You must specify at least
-- the 'SourceEnvironmentID' or the 'SourceEnvironmentName'. You may also
-- specify both. If you specify the 'SourceEnvironmentName', you must
-- specify the 'DestinationEnvironmentName'.
secnameSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentName =
    lens _secnameSourceEnvironmentName
        (\s a -> s { _secnameSourceEnvironmentName = a })

data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SwapEnvironmentCNAMEsResponse' constructor.
swapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse
swapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse

instance ToPath SwapEnvironmentCNAMEs where
    toPath = const "/"

instance ToQuery SwapEnvironmentCNAMEs where
    toQuery SwapEnvironmentCNAMEs{..} = mconcat
        [ "DestinationEnvironmentId"   =? _secnameDestinationEnvironmentId
        , "DestinationEnvironmentName" =? _secnameDestinationEnvironmentName
        , "SourceEnvironmentId"        =? _secnameSourceEnvironmentId
        , "SourceEnvironmentName"      =? _secnameSourceEnvironmentName
        ]

instance ToHeaders SwapEnvironmentCNAMEs

instance AWSRequest SwapEnvironmentCNAMEs where
    type Sv SwapEnvironmentCNAMEs = ElasticBeanstalk
    type Rs SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEsResponse

    request  = post "SwapEnvironmentCNAMEs"
    response = nullResponse SwapEnvironmentCNAMEsResponse
