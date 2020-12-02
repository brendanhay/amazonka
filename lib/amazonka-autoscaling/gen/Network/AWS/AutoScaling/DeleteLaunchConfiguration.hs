{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified launch configuration.
--
--
-- The launch configuration must not be attached to an Auto Scaling group. When this call completes, the launch configuration is no longer available for use.
--
module Network.AWS.AutoScaling.DeleteLaunchConfiguration
    (
    -- * Creating a Request
      deleteLaunchConfiguration
    , DeleteLaunchConfiguration
    -- * Request Lenses
    , dlcLaunchConfigurationName

    -- * Destructuring the Response
    , deleteLaunchConfigurationResponse
    , DeleteLaunchConfigurationResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLaunchConfiguration' smart constructor.
newtype DeleteLaunchConfiguration = DeleteLaunchConfiguration'
  { _dlcLaunchConfigurationName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcLaunchConfigurationName' - The name of the launch configuration.
deleteLaunchConfiguration
    :: Text -- ^ 'dlcLaunchConfigurationName'
    -> DeleteLaunchConfiguration
deleteLaunchConfiguration pLaunchConfigurationName_ =
  DeleteLaunchConfiguration'
    {_dlcLaunchConfigurationName = pLaunchConfigurationName_}


-- | The name of the launch configuration.
dlcLaunchConfigurationName :: Lens' DeleteLaunchConfiguration Text
dlcLaunchConfigurationName = lens _dlcLaunchConfigurationName (\ s a -> s{_dlcLaunchConfigurationName = a})

instance AWSRequest DeleteLaunchConfiguration where
        type Rs DeleteLaunchConfiguration =
             DeleteLaunchConfigurationResponse
        request = postQuery autoScaling
        response
          = receiveNull DeleteLaunchConfigurationResponse'

instance Hashable DeleteLaunchConfiguration where

instance NFData DeleteLaunchConfiguration where

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
data DeleteLaunchConfigurationResponse =
  DeleteLaunchConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLaunchConfigurationResponse' with the minimum fields required to make a request.
--
deleteLaunchConfigurationResponse
    :: DeleteLaunchConfigurationResponse
deleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse'


instance NFData DeleteLaunchConfigurationResponse
         where
