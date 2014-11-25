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

-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified launch configuration.
--
-- The launch configuration must not be attached to an Auto Scaling group. When
-- this call completes, the launch configuration is no longer available for use.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteLaunchConfiguration.html>
module Network.AWS.AutoScaling.DeleteLaunchConfiguration
    (
    -- * Request
      DeleteLaunchConfiguration
    -- ** Request constructor
    , deleteLaunchConfiguration
    -- ** Request lenses
    , dlcLaunchConfigurationName

    -- * Response
    , DeleteLaunchConfigurationResponse
    -- ** Response constructor
    , deleteLaunchConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

newtype DeleteLaunchConfiguration = DeleteLaunchConfiguration
    { _dlcLaunchConfigurationName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteLaunchConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcLaunchConfigurationName' @::@ 'Text'
--
deleteLaunchConfiguration :: Text -- ^ 'dlcLaunchConfigurationName'
                          -> DeleteLaunchConfiguration
deleteLaunchConfiguration p1 = DeleteLaunchConfiguration
    { _dlcLaunchConfigurationName = p1
    }

-- | The name of the launch configuration.
--
dlcLaunchConfigurationName :: Lens' DeleteLaunchConfiguration Text
dlcLaunchConfigurationName =
    lens _dlcLaunchConfigurationName
        (\s a -> s { _dlcLaunchConfigurationName = a })

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLaunchConfigurationResponse' constructor.
deleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse
deleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse

instance ToPath DeleteLaunchConfiguration where
    toPath = const "/"

instance ToQuery DeleteLaunchConfiguration where
    toQuery DeleteLaunchConfiguration{..} = mconcat
        [ "LaunchConfigurationName" =? _dlcLaunchConfigurationName
        ]

instance ToHeaders DeleteLaunchConfiguration

instance AWSRequest DeleteLaunchConfiguration where
    type Sv DeleteLaunchConfiguration = AutoScaling
    type Rs DeleteLaunchConfiguration = DeleteLaunchConfigurationResponse

    request  = post "DeleteLaunchConfiguration"
    response = nullResponse DeleteLaunchConfigurationResponse
