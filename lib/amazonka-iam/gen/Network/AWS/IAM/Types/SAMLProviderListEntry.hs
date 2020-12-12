{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SAMLProviderListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SAMLProviderListEntry
  ( SAMLProviderListEntry (..),

    -- * Smart constructor
    mkSAMLProviderListEntry,

    -- * Lenses
    samlpleARN,
    samlpleCreateDate,
    samlpleValidUntil,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the list of SAML providers for this account.
--
-- /See:/ 'mkSAMLProviderListEntry' smart constructor.
data SAMLProviderListEntry = SAMLProviderListEntry'
  { arn ::
      Lude.Maybe Lude.Text,
    createDate :: Lude.Maybe Lude.DateTime,
    validUntil :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SAMLProviderListEntry' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the SAML provider.
-- * 'createDate' - The date and time when the SAML provider was created.
-- * 'validUntil' - The expiration date and time for the SAML provider.
mkSAMLProviderListEntry ::
  SAMLProviderListEntry
mkSAMLProviderListEntry =
  SAMLProviderListEntry'
    { arn = Lude.Nothing,
      createDate = Lude.Nothing,
      validUntil = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the SAML provider.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlpleARN :: Lens.Lens' SAMLProviderListEntry (Lude.Maybe Lude.Text)
samlpleARN = Lens.lens (arn :: SAMLProviderListEntry -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SAMLProviderListEntry)
{-# DEPRECATED samlpleARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time when the SAML provider was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlpleCreateDate :: Lens.Lens' SAMLProviderListEntry (Lude.Maybe Lude.DateTime)
samlpleCreateDate = Lens.lens (createDate :: SAMLProviderListEntry -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: SAMLProviderListEntry)
{-# DEPRECATED samlpleCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The expiration date and time for the SAML provider.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlpleValidUntil :: Lens.Lens' SAMLProviderListEntry (Lude.Maybe Lude.DateTime)
samlpleValidUntil = Lens.lens (validUntil :: SAMLProviderListEntry -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: SAMLProviderListEntry)
{-# DEPRECATED samlpleValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

instance Lude.FromXML SAMLProviderListEntry where
  parseXML x =
    SAMLProviderListEntry'
      Lude.<$> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "ValidUntil")
