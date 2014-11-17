{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new configuration recorder to record the resource configurations.
-- You can use this action to change the role (roleARN) of an existing
-- recorder. To change the role, call the action on the existing configuration
-- recorder and specify a role.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_PutConfigurationRecorder.html>
module Network.AWS.Config.PutConfigurationRecorder
    (
    -- * Request
      PutConfigurationRecorder
    -- ** Request constructor
    , putConfigurationRecorder
    -- ** Request lenses
    , pcrConfigurationRecorder

    -- * Response
    , PutConfigurationRecorderResponse
    -- ** Response constructor
    , putConfigurationRecorderResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

newtype PutConfigurationRecorder = PutConfigurationRecorder
    { _pcrConfigurationRecorder :: ConfigurationRecorder
    } deriving (Eq, Show, Generic)

-- | 'PutConfigurationRecorder' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pcrConfigurationRecorder' @::@ 'ConfigurationRecorder'
--
putConfigurationRecorder :: ConfigurationRecorder -- ^ 'pcrConfigurationRecorder'
                         -> PutConfigurationRecorder
putConfigurationRecorder p1 = PutConfigurationRecorder
    { _pcrConfigurationRecorder = p1
    }

-- | The configuration recorder object that records each configuration change
-- made to the resources.
pcrConfigurationRecorder :: Lens' PutConfigurationRecorder ConfigurationRecorder
pcrConfigurationRecorder =
    lens _pcrConfigurationRecorder
        (\s a -> s { _pcrConfigurationRecorder = a })

data PutConfigurationRecorderResponse = PutConfigurationRecorderResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutConfigurationRecorderResponse' constructor.
putConfigurationRecorderResponse :: PutConfigurationRecorderResponse
putConfigurationRecorderResponse = PutConfigurationRecorderResponse

instance ToPath PutConfigurationRecorder where
    toPath = const "/"

instance ToQuery PutConfigurationRecorder where
    toQuery = const mempty

instance ToHeaders PutConfigurationRecorder
instance ToJSON PutConfigurationRecorder where
    toJSON = genericToJSON jsonOptions

instance AWSRequest PutConfigurationRecorder where
    type Sv PutConfigurationRecorder = Config
    type Rs PutConfigurationRecorder = PutConfigurationRecorderResponse

    request  = post "PutConfigurationRecorder"
    response = nullResponse PutConfigurationRecorderResponse
