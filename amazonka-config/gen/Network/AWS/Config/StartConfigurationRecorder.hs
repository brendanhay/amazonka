{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts recording configurations of all the resources associated with the
-- account. You must have created at least one delivery channel to
-- successfully start the configuration recorder.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_StartConfigurationRecorder.html>
module Network.AWS.Config.StartConfigurationRecorder
    (
    -- * Request
      StartConfigurationRecorder
    -- ** Request constructor
    , startConfigurationRecorder
    -- ** Request lenses
    , scr1ConfigurationRecorderName

    -- * Response
    , StartConfigurationRecorderResponse
    -- ** Response constructor
    , startConfigurationRecorderResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

newtype StartConfigurationRecorder = StartConfigurationRecorder
    { _scr1ConfigurationRecorderName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StartConfigurationRecorder' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scr1ConfigurationRecorderName' @::@ 'Text'
--
startConfigurationRecorder :: Text -- ^ 'scr1ConfigurationRecorderName'
                           -> StartConfigurationRecorder
startConfigurationRecorder p1 = StartConfigurationRecorder
    { _scr1ConfigurationRecorderName = p1
    }

-- | The name of the recorder object that records each configuration change
-- made to the resources.
scr1ConfigurationRecorderName :: Lens' StartConfigurationRecorder Text
scr1ConfigurationRecorderName =
    lens _scr1ConfigurationRecorderName
        (\s a -> s { _scr1ConfigurationRecorderName = a })

data StartConfigurationRecorderResponse = StartConfigurationRecorderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StartConfigurationRecorderResponse' constructor.
startConfigurationRecorderResponse :: StartConfigurationRecorderResponse
startConfigurationRecorderResponse = StartConfigurationRecorderResponse

instance ToPath StartConfigurationRecorder where
    toPath = const "/"

instance ToQuery StartConfigurationRecorder where
    toQuery = const mempty

instance ToHeaders StartConfigurationRecorder

instance ToJSON StartConfigurationRecorder where
    toJSON StartConfigurationRecorder{..} = object
        [ "ConfigurationRecorderName" .= _scr1ConfigurationRecorderName
        ]

instance AWSRequest StartConfigurationRecorder where
    type Sv StartConfigurationRecorder = Config
    type Rs StartConfigurationRecorder = StartConfigurationRecorderResponse

    request  = post "StartConfigurationRecorder"
    response = nullResponse StartConfigurationRecorderResponse
