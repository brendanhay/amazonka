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

-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified LaunchConfiguration. The specified launch
-- configuration must not be attached to an Auto Scaling group. When this call
-- completes, the launch configuration is no longer available for use.
module Network.AWS.AutoScaling.DeleteLaunchConfiguration
    (
    -- * Request
      LaunchConfigurationNameType
    -- ** Request constructor
    , launchConfigurationNameType
    -- ** Request lenses
    , lcntLaunchConfigurationName

    -- * Response
    , DeleteLaunchConfigurationResponse
    -- ** Response constructor
    , deleteLaunchConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

newtype LaunchConfigurationNameType = LaunchConfigurationNameType
    { _lcntLaunchConfigurationName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'LaunchConfigurationNameType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcntLaunchConfigurationName' @::@ 'Text'
--
launchConfigurationNameType :: Text -- ^ 'lcntLaunchConfigurationName'
                            -> LaunchConfigurationNameType
launchConfigurationNameType p1 = LaunchConfigurationNameType
    { _lcntLaunchConfigurationName = p1
    }

-- | The name of the launch configuration.
lcntLaunchConfigurationName :: Lens' LaunchConfigurationNameType Text
lcntLaunchConfigurationName =
    lens _lcntLaunchConfigurationName
        (\s a -> s { _lcntLaunchConfigurationName = a })

instance ToQuery LaunchConfigurationNameType

instance ToPath LaunchConfigurationNameType where
    toPath = const "/"

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLaunchConfigurationResponse' constructor.
deleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse
deleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse

instance FromXML DeleteLaunchConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteLaunchConfigurationResponse"

instance AWSRequest LaunchConfigurationNameType where
    type Sv LaunchConfigurationNameType = AutoScaling
    type Rs LaunchConfigurationNameType = DeleteLaunchConfigurationResponse

    request  = post "DeleteLaunchConfiguration"
    response = nullaryResponse DeleteLaunchConfigurationResponse
