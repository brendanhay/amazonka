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

-- Module      : Network.AWS.Config.StopConfigurationRecorder
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops recording configurations of all the resources associated with the
-- account.
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Config.Types

newtype StopConfigurationRecorder = StopConfigurationRecorder
    { _scrConfigurationRecorderName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

-- | The name of the recorder object that records each configuration change
-- made to the resources.
scrConfigurationRecorderName :: Lens' StopConfigurationRecorder Text
scrConfigurationRecorderName =
    lens _scrConfigurationRecorderName
        (\s a -> s { _scrConfigurationRecorderName = a })

instance ToPath StopConfigurationRecorder where
    toPath = const "/"

instance ToQuery StopConfigurationRecorder where
    toQuery = const mempty

instance ToHeaders StopConfigurationRecorder

instance ToBody StopConfigurationRecorder where
    toBody = toBody . encode . _scrConfigurationRecorderName

data StopConfigurationRecorderResponse = StopConfigurationRecorderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StopConfigurationRecorderResponse' constructor.
stopConfigurationRecorderResponse :: StopConfigurationRecorderResponse
stopConfigurationRecorderResponse = StopConfigurationRecorderResponse

instance AWSRequest StopConfigurationRecorder where
    type Sv StopConfigurationRecorder = Config
    type Rs StopConfigurationRecorder = StopConfigurationRecorderResponse

    request  = post
    response = nullaryResponse StopConfigurationRecorderResponse
