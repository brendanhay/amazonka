{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IKEVersionsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IKEVersionsRequestListValue
  ( IKEVersionsRequestListValue (..),

    -- * Smart constructor
    mkIKEVersionsRequestListValue,

    -- * Lenses
    ikevrlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The IKE version that is permitted for the VPN tunnel.
--
-- /See:/ 'mkIKEVersionsRequestListValue' smart constructor.
newtype IKEVersionsRequestListValue = IKEVersionsRequestListValue'
  { value ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IKEVersionsRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The IKE version.
mkIKEVersionsRequestListValue ::
  IKEVersionsRequestListValue
mkIKEVersionsRequestListValue =
  IKEVersionsRequestListValue' {value = Lude.Nothing}

-- | The IKE version.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikevrlvValue :: Lens.Lens' IKEVersionsRequestListValue (Lude.Maybe Lude.Text)
ikevrlvValue = Lens.lens (value :: IKEVersionsRequestListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: IKEVersionsRequestListValue)
{-# DEPRECATED ikevrlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery IKEVersionsRequestListValue where
  toQuery IKEVersionsRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
