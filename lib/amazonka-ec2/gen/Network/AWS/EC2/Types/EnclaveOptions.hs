-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnclaveOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnclaveOptions
  ( EnclaveOptions (..),

    -- * Smart constructor
    mkEnclaveOptions,

    -- * Lenses
    eoEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /See:/ 'mkEnclaveOptions' smart constructor.
newtype EnclaveOptions = EnclaveOptions'
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

-- | Creates a value of 'EnclaveOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
mkEnclaveOptions ::
  EnclaveOptions
mkEnclaveOptions = EnclaveOptions' {enabled = Lude.Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoEnabled :: Lens.Lens' EnclaveOptions (Lude.Maybe Lude.Bool)
eoEnabled = Lens.lens (enabled :: EnclaveOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EnclaveOptions)
{-# DEPRECATED eoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML EnclaveOptions where
  parseXML x = EnclaveOptions' Lude.<$> (x Lude..@? "enabled")
