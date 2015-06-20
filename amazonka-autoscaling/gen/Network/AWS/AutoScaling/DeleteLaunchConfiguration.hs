{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified launch configuration.
--
-- The launch configuration must not be attached to an Auto Scaling group.
-- When this call completes, the launch configuration is no longer
-- available for use.
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

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLaunchConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcLaunchConfigurationName'
newtype DeleteLaunchConfiguration = DeleteLaunchConfiguration'{_dlcLaunchConfigurationName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteLaunchConfiguration' smart constructor.
deleteLaunchConfiguration :: Text -> DeleteLaunchConfiguration
deleteLaunchConfiguration pLaunchConfigurationName = DeleteLaunchConfiguration'{_dlcLaunchConfigurationName = pLaunchConfigurationName};

-- | The name of the launch configuration.
dlcLaunchConfigurationName :: Lens' DeleteLaunchConfiguration Text
dlcLaunchConfigurationName = lens _dlcLaunchConfigurationName (\ s a -> s{_dlcLaunchConfigurationName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteLaunchConfiguration where
        type Sv DeleteLaunchConfiguration = AutoScaling
        type Rs DeleteLaunchConfiguration =
             DeleteLaunchConfigurationResponse
        request = post
        response
          = receiveNull DeleteLaunchConfigurationResponse'

instance ToHeaders DeleteLaunchConfiguration where
        toHeaders = const mempty

instance ToPath DeleteLaunchConfiguration where
        toPath = const "/"

instance ToQuery DeleteLaunchConfiguration where
        toQuery DeleteLaunchConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("DeleteLaunchConfiguration" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LaunchConfigurationName" =:
                 _dlcLaunchConfigurationName]

-- | /See:/ 'deleteLaunchConfigurationResponse' smart constructor.
data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse' deriving (Eq, Read, Show)

-- | 'DeleteLaunchConfigurationResponse' smart constructor.
deleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse
deleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse';
