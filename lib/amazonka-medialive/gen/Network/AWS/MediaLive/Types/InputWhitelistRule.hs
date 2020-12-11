-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputWhitelistRule
  ( InputWhitelistRule (..),

    -- * Smart constructor
    mkInputWhitelistRule,

    -- * Lenses
    iwrCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Whitelist rule
--
-- /See:/ 'mkInputWhitelistRule' smart constructor.
newtype InputWhitelistRule = InputWhitelistRule'
  { cidr ::
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

-- | Creates a value of 'InputWhitelistRule' with the minimum fields required to make a request.
--
-- * 'cidr' - The IPv4 CIDR that's whitelisted.
mkInputWhitelistRule ::
  InputWhitelistRule
mkInputWhitelistRule = InputWhitelistRule' {cidr = Lude.Nothing}

-- | The IPv4 CIDR that's whitelisted.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwrCidr :: Lens.Lens' InputWhitelistRule (Lude.Maybe Lude.Text)
iwrCidr = Lens.lens (cidr :: InputWhitelistRule -> Lude.Maybe Lude.Text) (\s a -> s {cidr = a} :: InputWhitelistRule)
{-# DEPRECATED iwrCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.FromJSON InputWhitelistRule where
  parseJSON =
    Lude.withObject
      "InputWhitelistRule"
      (\x -> InputWhitelistRule' Lude.<$> (x Lude..:? "cidr"))
