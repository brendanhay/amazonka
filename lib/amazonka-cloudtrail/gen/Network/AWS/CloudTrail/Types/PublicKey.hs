{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.PublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.PublicKey
  ( PublicKey (..),

    -- * Smart constructor
    mkPublicKey,

    -- * Lenses
    pkFingerprint,
    pkValidityEndTime,
    pkValue,
    pkValidityStartTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a returned public key.
--
-- /See:/ 'mkPublicKey' smart constructor.
data PublicKey = PublicKey'
  { fingerprint :: Lude.Maybe Lude.Text,
    validityEndTime :: Lude.Maybe Lude.Timestamp,
    value :: Lude.Maybe Lude.Base64,
    validityStartTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- * 'fingerprint' - The fingerprint of the public key.
-- * 'validityEndTime' - The ending time of validity of the public key.
-- * 'validityStartTime' - The starting time of validity of the public key.
-- * 'value' - The DER encoded public key value in PKCS#1 format.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mkPublicKey ::
  PublicKey
mkPublicKey =
  PublicKey'
    { fingerprint = Lude.Nothing,
      validityEndTime = Lude.Nothing,
      value = Lude.Nothing,
      validityStartTime = Lude.Nothing
    }

-- | The fingerprint of the public key.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkFingerprint :: Lens.Lens' PublicKey (Lude.Maybe Lude.Text)
pkFingerprint = Lens.lens (fingerprint :: PublicKey -> Lude.Maybe Lude.Text) (\s a -> s {fingerprint = a} :: PublicKey)
{-# DEPRECATED pkFingerprint "Use generic-lens or generic-optics with 'fingerprint' instead." #-}

-- | The ending time of validity of the public key.
--
-- /Note:/ Consider using 'validityEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkValidityEndTime :: Lens.Lens' PublicKey (Lude.Maybe Lude.Timestamp)
pkValidityEndTime = Lens.lens (validityEndTime :: PublicKey -> Lude.Maybe Lude.Timestamp) (\s a -> s {validityEndTime = a} :: PublicKey)
{-# DEPRECATED pkValidityEndTime "Use generic-lens or generic-optics with 'validityEndTime' instead." #-}

-- | The DER encoded public key value in PKCS#1 format.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkValue :: Lens.Lens' PublicKey (Lude.Maybe Lude.Base64)
pkValue = Lens.lens (value :: PublicKey -> Lude.Maybe Lude.Base64) (\s a -> s {value = a} :: PublicKey)
{-# DEPRECATED pkValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The starting time of validity of the public key.
--
-- /Note:/ Consider using 'validityStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkValidityStartTime :: Lens.Lens' PublicKey (Lude.Maybe Lude.Timestamp)
pkValidityStartTime = Lens.lens (validityStartTime :: PublicKey -> Lude.Maybe Lude.Timestamp) (\s a -> s {validityStartTime = a} :: PublicKey)
{-# DEPRECATED pkValidityStartTime "Use generic-lens or generic-optics with 'validityStartTime' instead." #-}

instance Lude.FromJSON PublicKey where
  parseJSON =
    Lude.withObject
      "PublicKey"
      ( \x ->
          PublicKey'
            Lude.<$> (x Lude..:? "Fingerprint")
            Lude.<*> (x Lude..:? "ValidityEndTime")
            Lude.<*> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "ValidityStartTime")
      )
