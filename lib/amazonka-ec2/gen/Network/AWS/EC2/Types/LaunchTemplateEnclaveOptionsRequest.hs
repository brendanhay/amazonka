-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
  ( LaunchTemplateEnclaveOptionsRequest (..),

    -- * Smart constructor
    mkLaunchTemplateEnclaveOptionsRequest,

    -- * Lenses
    lteorEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- /See:/ 'mkLaunchTemplateEnclaveOptionsRequest' smart constructor.
newtype LaunchTemplateEnclaveOptionsRequest = LaunchTemplateEnclaveOptionsRequest'
  { enabled ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateEnclaveOptionsRequest' with the minimum fields required to make a request.
--
-- * 'enabled' - To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
mkLaunchTemplateEnclaveOptionsRequest ::
  LaunchTemplateEnclaveOptionsRequest
mkLaunchTemplateEnclaveOptionsRequest =
  LaunchTemplateEnclaveOptionsRequest' {enabled = Lude.Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteorEnabled :: Lens.Lens' LaunchTemplateEnclaveOptionsRequest (Lude.Maybe Lude.Bool)
lteorEnabled = Lens.lens (enabled :: LaunchTemplateEnclaveOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: LaunchTemplateEnclaveOptionsRequest)
{-# DEPRECATED lteorEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToQuery LaunchTemplateEnclaveOptionsRequest where
  toQuery LaunchTemplateEnclaveOptionsRequest' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
