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
    , mkDeleteLaunchConfiguration
    -- ** Request lenses
    , dlcLaunchConfigurationName

    -- * Response
    , DeleteLaunchConfigurationResponse
    -- ** Response constructor
    , mkDeleteLaunchConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
newtype DeleteLaunchConfiguration = DeleteLaunchConfiguration
    { _dlcLaunchConfigurationName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLaunchConfiguration' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LaunchConfigurationName ::@ @Text@
--
mkDeleteLaunchConfiguration :: Text -- ^ 'dlcLaunchConfigurationName'
                            -> DeleteLaunchConfiguration
mkDeleteLaunchConfiguration p1 = DeleteLaunchConfiguration
    { _dlcLaunchConfigurationName = p1
    }

-- | The name of the launch configuration.
dlcLaunchConfigurationName :: Lens' DeleteLaunchConfiguration Text
dlcLaunchConfigurationName =
    lens _dlcLaunchConfigurationName
         (\s a -> s { _dlcLaunchConfigurationName = a })

instance ToQuery DeleteLaunchConfiguration where
    toQuery = genericQuery def

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLaunchConfigurationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse
mkDeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse

instance AWSRequest DeleteLaunchConfiguration where
    type Sv DeleteLaunchConfiguration = AutoScaling
    type Rs DeleteLaunchConfiguration = DeleteLaunchConfigurationResponse

    request = post "DeleteLaunchConfiguration"
    response _ = nullaryResponse DeleteLaunchConfigurationResponse
