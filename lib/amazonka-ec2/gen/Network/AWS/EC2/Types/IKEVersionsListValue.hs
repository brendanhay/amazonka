{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IKEVersionsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IKEVersionsListValue
  ( IKEVersionsListValue (..),

    -- * Smart constructor
    mkIKEVersionsListValue,

    -- * Lenses
    ikevlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The internet key exchange (IKE) version permitted for the VPN tunnel.
--
-- /See:/ 'mkIKEVersionsListValue' smart constructor.
newtype IKEVersionsListValue = IKEVersionsListValue'
  { -- | The IKE version.
    value :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IKEVersionsListValue' with the minimum fields required to make a request.
--
-- * 'value' - The IKE version.
mkIKEVersionsListValue ::
  IKEVersionsListValue
mkIKEVersionsListValue =
  IKEVersionsListValue' {value = Lude.Nothing}

-- | The IKE version.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikevlvValue :: Lens.Lens' IKEVersionsListValue (Lude.Maybe Lude.Text)
ikevlvValue = Lens.lens (value :: IKEVersionsListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: IKEVersionsListValue)
{-# DEPRECATED ikevlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML IKEVersionsListValue where
  parseXML x = IKEVersionsListValue' Lude.<$> (x Lude..@? "value")
