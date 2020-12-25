{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
  ( CreateRemoteAccessSessionConfiguration (..),

    -- * Smart constructor
    mkCreateRemoteAccessSessionConfiguration,

    -- * Lenses
    crascBillingMethod,
    crascVpceConfigurationArns,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.BillingMethod as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration settings for a remote access session, including billing method.
--
-- /See:/ 'mkCreateRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { -- | The billing method for the remote access session.
    billingMethod :: Core.Maybe Types.BillingMethod,
    -- | An array of ARNs included in the VPC endpoint configuration.
    vpceConfigurationArns :: Core.Maybe [Types.AmazonResourceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRemoteAccessSessionConfiguration' value with any optional fields omitted.
mkCreateRemoteAccessSessionConfiguration ::
  CreateRemoteAccessSessionConfiguration
mkCreateRemoteAccessSessionConfiguration =
  CreateRemoteAccessSessionConfiguration'
    { billingMethod =
        Core.Nothing,
      vpceConfigurationArns = Core.Nothing
    }

-- | The billing method for the remote access session.
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crascBillingMethod :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Core.Maybe Types.BillingMethod)
crascBillingMethod = Lens.field @"billingMethod"
{-# DEPRECATED crascBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | An array of ARNs included in the VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crascVpceConfigurationArns :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Core.Maybe [Types.AmazonResourceName])
crascVpceConfigurationArns = Lens.field @"vpceConfigurationArns"
{-# DEPRECATED crascVpceConfigurationArns "Use generic-lens or generic-optics with 'vpceConfigurationArns' instead." #-}

instance Core.FromJSON CreateRemoteAccessSessionConfiguration where
  toJSON CreateRemoteAccessSessionConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingMethod" Core..=) Core.<$> billingMethod,
            ("vpceConfigurationArns" Core..=) Core.<$> vpceConfigurationArns
          ]
      )
