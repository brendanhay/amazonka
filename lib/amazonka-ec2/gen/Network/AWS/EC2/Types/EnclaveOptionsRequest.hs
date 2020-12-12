{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnclaveOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnclaveOptionsRequest
  ( EnclaveOptionsRequest (..),

    -- * Smart constructor
    mkEnclaveOptionsRequest,

    -- * Lenses
    eorEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?> in the /AWS Nitro Enclaves User Guide/ .
--
-- /See:/ 'mkEnclaveOptionsRequest' smart constructor.
newtype EnclaveOptionsRequest = EnclaveOptionsRequest'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnclaveOptionsRequest' with the minimum fields required to make a request.
--
-- * 'enabled' - To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
mkEnclaveOptionsRequest ::
  EnclaveOptionsRequest
mkEnclaveOptionsRequest =
  EnclaveOptionsRequest' {enabled = Lude.Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ .
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eorEnabled :: Lens.Lens' EnclaveOptionsRequest (Lude.Maybe Lude.Bool)
eorEnabled = Lens.lens (enabled :: EnclaveOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EnclaveOptionsRequest)
{-# DEPRECATED eorEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToQuery EnclaveOptionsRequest where
  toQuery EnclaveOptionsRequest' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
