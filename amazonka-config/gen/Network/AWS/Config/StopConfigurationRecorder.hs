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

-- Module      : Network.AWS.Config.StopConfigurationRecorder
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

-- | Stops recording configurations of all the resources associated with the
-- account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_StopConfigurationRecorder.html>
module Network.AWS.Config.StopConfigurationRecorder
    (
    -- * Request
      StopConfigurationRecorder
    -- ** Request constructor
    , stopConfigurationRecorder
    -- ** Request lenses
    , scrConfigurationRecorderName

    -- * Response
    , StopConfigurationRecorderResponse
    -- ** Response constructor
    , stopConfigurationRecorderResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

newtype StopConfigurationRecorder = StopConfigurationRecorder
    { _scrConfigurationRecorderName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'StopConfigurationRecorder' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scrConfigurationRecorderName' @::@ 'Text'
--
stopConfigurationRecorder :: Text -- ^ 'scrConfigurationRecorderName'
                          -> StopConfigurationRecorder
stopConfigurationRecorder p1 = StopConfigurationRecorder
    { _scrConfigurationRecorderName = p1
    }

-- | The name of the recorder object that records each configuration change made
-- to the resources.
scrConfigurationRecorderName :: Lens' StopConfigurationRecorder Text
scrConfigurationRecorderName =
    lens _scrConfigurationRecorderName
        (\s a -> s { _scrConfigurationRecorderName = a })

data StopConfigurationRecorderResponse = StopConfigurationRecorderResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'StopConfigurationRecorderResponse' constructor.
stopConfigurationRecorderResponse :: StopConfigurationRecorderResponse
stopConfigurationRecorderResponse = StopConfigurationRecorderResponse

instance ToPath StopConfigurationRecorder where
    toPath = const "/"

instance ToQuery StopConfigurationRecorder where
    toQuery = const mempty

instance ToHeaders StopConfigurationRecorder

instance ToJSON StopConfigurationRecorder where
    toJSON StopConfigurationRecorder{..} = object
        [ "ConfigurationRecorderName" .= _scrConfigurationRecorderName
        ]

instance AWSRequest StopConfigurationRecorder where
    type Sv StopConfigurationRecorder = Config
    type Rs StopConfigurationRecorder = StopConfigurationRecorderResponse

    request  = post "StopConfigurationRecorder"
    response = nullResponse StopConfigurationRecorderResponse
