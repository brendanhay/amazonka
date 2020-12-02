{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the service configuration associated with the specified resource group. AWS Resource Groups supports configurations for the following resource group types:
--
--
--     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
module Network.AWS.ResourceGroups.GetGroupConfiguration
  ( -- * Creating a Request
    getGroupConfiguration,
    GetGroupConfiguration,

    -- * Request Lenses
    ggcGroup,

    -- * Destructuring the Response
    getGroupConfigurationResponse,
    GetGroupConfigurationResponse,

    -- * Response Lenses
    ggcrsGroupConfiguration,
    ggcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'getGroupConfiguration' smart constructor.
newtype GetGroupConfiguration = GetGroupConfiguration'
  { _ggcGroup ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggcGroup' - The name or the ARN of the resource group.
getGroupConfiguration ::
  GetGroupConfiguration
getGroupConfiguration = GetGroupConfiguration' {_ggcGroup = Nothing}

-- | The name or the ARN of the resource group.
ggcGroup :: Lens' GetGroupConfiguration (Maybe Text)
ggcGroup = lens _ggcGroup (\s a -> s {_ggcGroup = a})

instance AWSRequest GetGroupConfiguration where
  type Rs GetGroupConfiguration = GetGroupConfigurationResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          GetGroupConfigurationResponse'
            <$> (x .?> "GroupConfiguration") <*> (pure (fromEnum s))
      )

instance Hashable GetGroupConfiguration

instance NFData GetGroupConfiguration

instance ToHeaders GetGroupConfiguration where
  toHeaders = const mempty

instance ToJSON GetGroupConfiguration where
  toJSON GetGroupConfiguration' {..} =
    object (catMaybes [("Group" .=) <$> _ggcGroup])

instance ToPath GetGroupConfiguration where
  toPath = const "/get-group-configuration"

instance ToQuery GetGroupConfiguration where
  toQuery = const mempty

-- | /See:/ 'getGroupConfigurationResponse' smart constructor.
data GetGroupConfigurationResponse = GetGroupConfigurationResponse'
  { _ggcrsGroupConfiguration ::
      !(Maybe GroupConfiguration),
    _ggcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggcrsGroupConfiguration' - The configuration associated with the specified group.
--
-- * 'ggcrsResponseStatus' - -- | The response status code.
getGroupConfigurationResponse ::
  -- | 'ggcrsResponseStatus'
  Int ->
  GetGroupConfigurationResponse
getGroupConfigurationResponse pResponseStatus_ =
  GetGroupConfigurationResponse'
    { _ggcrsGroupConfiguration =
        Nothing,
      _ggcrsResponseStatus = pResponseStatus_
    }

-- | The configuration associated with the specified group.
ggcrsGroupConfiguration :: Lens' GetGroupConfigurationResponse (Maybe GroupConfiguration)
ggcrsGroupConfiguration = lens _ggcrsGroupConfiguration (\s a -> s {_ggcrsGroupConfiguration = a})

-- | -- | The response status code.
ggcrsResponseStatus :: Lens' GetGroupConfigurationResponse Int
ggcrsResponseStatus = lens _ggcrsResponseStatus (\s a -> s {_ggcrsResponseStatus = a})

instance NFData GetGroupConfigurationResponse
