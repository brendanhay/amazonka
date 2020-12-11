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
    crascVpceConfigurationARNs,
  )
where

import Network.AWS.DeviceFarm.Types.BillingMethod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration settings for a remote access session, including billing method.
--
-- /See:/ 'mkCreateRemoteAccessSessionConfiguration' smart constructor.
data CreateRemoteAccessSessionConfiguration = CreateRemoteAccessSessionConfiguration'
  { billingMethod ::
      Lude.Maybe
        BillingMethod,
    vpceConfigurationARNs ::
      Lude.Maybe
        [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRemoteAccessSessionConfiguration' with the minimum fields required to make a request.
--
-- * 'billingMethod' - The billing method for the remote access session.
-- * 'vpceConfigurationARNs' - An array of ARNs included in the VPC endpoint configuration.
mkCreateRemoteAccessSessionConfiguration ::
  CreateRemoteAccessSessionConfiguration
mkCreateRemoteAccessSessionConfiguration =
  CreateRemoteAccessSessionConfiguration'
    { billingMethod =
        Lude.Nothing,
      vpceConfigurationARNs = Lude.Nothing
    }

-- | The billing method for the remote access session.
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crascBillingMethod :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Lude.Maybe BillingMethod)
crascBillingMethod = Lens.lens (billingMethod :: CreateRemoteAccessSessionConfiguration -> Lude.Maybe BillingMethod) (\s a -> s {billingMethod = a} :: CreateRemoteAccessSessionConfiguration)
{-# DEPRECATED crascBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | An array of ARNs included in the VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crascVpceConfigurationARNs :: Lens.Lens' CreateRemoteAccessSessionConfiguration (Lude.Maybe [Lude.Text])
crascVpceConfigurationARNs = Lens.lens (vpceConfigurationARNs :: CreateRemoteAccessSessionConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {vpceConfigurationARNs = a} :: CreateRemoteAccessSessionConfiguration)
{-# DEPRECATED crascVpceConfigurationARNs "Use generic-lens or generic-optics with 'vpceConfigurationARNs' instead." #-}

instance Lude.ToJSON CreateRemoteAccessSessionConfiguration where
  toJSON CreateRemoteAccessSessionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("billingMethod" Lude..=) Lude.<$> billingMethod,
            ("vpceConfigurationArns" Lude..=) Lude.<$> vpceConfigurationARNs
          ]
      )
