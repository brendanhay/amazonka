-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.OpenIdConnectProviderListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.OpenIdConnectProviderListEntry
  ( OpenIdConnectProviderListEntry (..),

    -- * Smart constructor
    mkOpenIdConnectProviderListEntry,

    -- * Lenses
    oicpleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect provider.
--
-- /See:/ 'mkOpenIdConnectProviderListEntry' smart constructor.
newtype OpenIdConnectProviderListEntry = OpenIdConnectProviderListEntry'
  { arn ::
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

-- | Creates a value of 'OpenIdConnectProviderListEntry' with the minimum fields required to make a request.
--
-- * 'arn' - Undocumented field.
mkOpenIdConnectProviderListEntry ::
  OpenIdConnectProviderListEntry
mkOpenIdConnectProviderListEntry =
  OpenIdConnectProviderListEntry' {arn = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oicpleARN :: Lens.Lens' OpenIdConnectProviderListEntry (Lude.Maybe Lude.Text)
oicpleARN = Lens.lens (arn :: OpenIdConnectProviderListEntry -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: OpenIdConnectProviderListEntry)
{-# DEPRECATED oicpleARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromXML OpenIdConnectProviderListEntry where
  parseXML x =
    OpenIdConnectProviderListEntry' Lude.<$> (x Lude..@? "Arn")
