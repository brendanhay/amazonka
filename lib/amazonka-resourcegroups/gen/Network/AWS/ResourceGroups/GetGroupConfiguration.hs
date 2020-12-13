{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    GetGroupConfiguration (..),
    mkGetGroupConfiguration,

    -- ** Request lenses
    ggcGroup,

    -- * Destructuring the response
    GetGroupConfigurationResponse (..),
    mkGetGroupConfigurationResponse,

    -- ** Response lenses
    ggcrsGroupConfiguration,
    ggcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupConfiguration' smart constructor.
newtype GetGroupConfiguration = GetGroupConfiguration'
  { -- | The name or the ARN of the resource group.
    group :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupConfiguration' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group.
mkGetGroupConfiguration ::
  GetGroupConfiguration
mkGetGroupConfiguration =
  GetGroupConfiguration' {group = Lude.Nothing}

-- | The name or the ARN of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcGroup :: Lens.Lens' GetGroupConfiguration (Lude.Maybe Lude.Text)
ggcGroup = Lens.lens (group :: GetGroupConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: GetGroupConfiguration)
{-# DEPRECATED ggcGroup "Use generic-lens or generic-optics with 'group' instead." #-}

instance Lude.AWSRequest GetGroupConfiguration where
  type Rs GetGroupConfiguration = GetGroupConfigurationResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupConfigurationResponse'
            Lude.<$> (x Lude..?> "GroupConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetGroupConfiguration where
  toJSON GetGroupConfiguration' {..} =
    Lude.object (Lude.catMaybes [("Group" Lude..=) Lude.<$> group])

instance Lude.ToPath GetGroupConfiguration where
  toPath = Lude.const "/get-group-configuration"

instance Lude.ToQuery GetGroupConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupConfigurationResponse' smart constructor.
data GetGroupConfigurationResponse = GetGroupConfigurationResponse'
  { -- | The configuration associated with the specified group.
    groupConfiguration :: Lude.Maybe GroupConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'groupConfiguration' - The configuration associated with the specified group.
-- * 'responseStatus' - The response status code.
mkGetGroupConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupConfigurationResponse
mkGetGroupConfigurationResponse pResponseStatus_ =
  GetGroupConfigurationResponse'
    { groupConfiguration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The configuration associated with the specified group.
--
-- /Note:/ Consider using 'groupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrsGroupConfiguration :: Lens.Lens' GetGroupConfigurationResponse (Lude.Maybe GroupConfiguration)
ggcrsGroupConfiguration = Lens.lens (groupConfiguration :: GetGroupConfigurationResponse -> Lude.Maybe GroupConfiguration) (\s a -> s {groupConfiguration = a} :: GetGroupConfigurationResponse)
{-# DEPRECATED ggcrsGroupConfiguration "Use generic-lens or generic-optics with 'groupConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrsResponseStatus :: Lens.Lens' GetGroupConfigurationResponse Lude.Int
ggcrsResponseStatus = Lens.lens (responseStatus :: GetGroupConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupConfigurationResponse)
{-# DEPRECATED ggcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
