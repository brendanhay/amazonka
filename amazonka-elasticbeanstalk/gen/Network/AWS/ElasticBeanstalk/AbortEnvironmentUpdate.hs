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

-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
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

-- | Cancels in-progress environment configuration update or application version
-- deployment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_AbortEnvironmentUpdate.html>
module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
    (
    -- * Request
      AbortEnvironmentUpdate
    -- ** Request constructor
    , abortEnvironmentUpdate
    -- ** Request lenses
    , aeuEnvironmentId
    , aeuEnvironmentName

    -- * Response
    , AbortEnvironmentUpdateResponse
    -- ** Response constructor
    , abortEnvironmentUpdateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data AbortEnvironmentUpdate = AbortEnvironmentUpdate
    { _aeuEnvironmentId   :: Maybe Text
    , _aeuEnvironmentName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AbortEnvironmentUpdate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeuEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'aeuEnvironmentName' @::@ 'Maybe' 'Text'
--
abortEnvironmentUpdate :: AbortEnvironmentUpdate
abortEnvironmentUpdate = AbortEnvironmentUpdate
    { _aeuEnvironmentId   = Nothing
    , _aeuEnvironmentName = Nothing
    }

-- | This specifies the ID of the environment with the in-progress update that you
-- want to cancel.
aeuEnvironmentId :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentId = lens _aeuEnvironmentId (\s a -> s { _aeuEnvironmentId = a })

-- | This specifies the name of the environment with the in-progress update that
-- you want to cancel.
aeuEnvironmentName :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentName =
    lens _aeuEnvironmentName (\s a -> s { _aeuEnvironmentName = a })

data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AbortEnvironmentUpdateResponse' constructor.
abortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse
abortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse

instance ToPath AbortEnvironmentUpdate where
    toPath = const "/"

instance ToQuery AbortEnvironmentUpdate where
    toQuery AbortEnvironmentUpdate{..} = mconcat
        [ "EnvironmentId"   =? _aeuEnvironmentId
        , "EnvironmentName" =? _aeuEnvironmentName
        ]

instance ToHeaders AbortEnvironmentUpdate

instance AWSRequest AbortEnvironmentUpdate where
    type Sv AbortEnvironmentUpdate = ElasticBeanstalk
    type Rs AbortEnvironmentUpdate = AbortEnvironmentUpdateResponse

    request  = post "AbortEnvironmentUpdate"
    response = nullResponse AbortEnvironmentUpdateResponse
