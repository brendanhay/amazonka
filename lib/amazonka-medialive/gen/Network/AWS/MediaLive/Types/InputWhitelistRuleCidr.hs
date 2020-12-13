{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
  ( InputWhitelistRuleCidr (..),

    -- * Smart constructor
    mkInputWhitelistRuleCidr,

    -- * Lenses
    iwrcCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An IPv4 CIDR to whitelist.
--
-- /See:/ 'mkInputWhitelistRuleCidr' smart constructor.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr'
  { -- | The IPv4 CIDR to whitelist.
    cidr :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputWhitelistRuleCidr' with the minimum fields required to make a request.
--
-- * 'cidr' - The IPv4 CIDR to whitelist.
mkInputWhitelistRuleCidr ::
  InputWhitelistRuleCidr
mkInputWhitelistRuleCidr =
  InputWhitelistRuleCidr' {cidr = Lude.Nothing}

-- | The IPv4 CIDR to whitelist.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwrcCidr :: Lens.Lens' InputWhitelistRuleCidr (Lude.Maybe Lude.Text)
iwrcCidr = Lens.lens (cidr :: InputWhitelistRuleCidr -> Lude.Maybe Lude.Text) (\s a -> s {cidr = a} :: InputWhitelistRuleCidr)
{-# DEPRECATED iwrcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.ToJSON InputWhitelistRuleCidr where
  toJSON InputWhitelistRuleCidr' {..} =
    Lude.object (Lude.catMaybes [("cidr" Lude..=) Lude.<$> cidr])
