{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteLaunchConfiguration
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
-- https://autoscaling.amazonaws.com/?LaunchConfigurationName=my-test-lc
-- &Version=2011-01-01 &Action=DeleteLaunchConfiguration &AUTHPARAMS
-- 7347261f-97df-11e2-8756-35eEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DeleteLaunchConfiguration
    (
    -- * Request
      DeleteLaunchConfiguration
    -- ** Request constructor
    , deleteLaunchConfiguration
    -- ** Request lenses
    , lcntLaunchConfigurationName

    -- * Response
    , DeleteLaunchConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteLaunchConfiguration' request.
deleteLaunchConfiguration :: Text -- ^ 'lcntLaunchConfigurationName'
                          -> DeleteLaunchConfiguration
deleteLaunchConfiguration p1 = DeleteLaunchConfiguration
    { _lcntLaunchConfigurationName = p1
    }

data DeleteLaunchConfiguration = DeleteLaunchConfiguration
    { _lcntLaunchConfigurationName :: Text
      -- ^ The name of the launch configuration.
    } deriving (Show, Generic)

-- | The name of the launch configuration.
lcntLaunchConfigurationName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteLaunchConfiguration
    -> f DeleteLaunchConfiguration
lcntLaunchConfigurationName f x =
    (\y -> x { _lcntLaunchConfigurationName = y })
       <$> f (_lcntLaunchConfigurationName x)
{-# INLINE lcntLaunchConfigurationName #-}

instance ToQuery DeleteLaunchConfiguration where
    toQuery = genericQuery def

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLaunchConfiguration where
    type Sv DeleteLaunchConfiguration = AutoScaling
    type Rs DeleteLaunchConfiguration = DeleteLaunchConfigurationResponse

    request = post "DeleteLaunchConfiguration"
    response _ = nullaryResponse DeleteLaunchConfigurationResponse
