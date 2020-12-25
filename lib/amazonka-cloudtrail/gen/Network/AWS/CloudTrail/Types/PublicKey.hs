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
    pkValidityStartTime,
    pkValue,
  )
where

import qualified Network.AWS.CloudTrail.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a returned public key.
--
-- /See:/ 'mkPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The fingerprint of the public key.
    fingerprint :: Core.Maybe Types.String,
    -- | The ending time of validity of the public key.
    validityEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The starting time of validity of the public key.
    validityStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The DER encoded public key value in PKCS#1 format.
    value :: Core.Maybe Core.Base64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PublicKey' value with any optional fields omitted.
mkPublicKey ::
  PublicKey
mkPublicKey =
  PublicKey'
    { fingerprint = Core.Nothing,
      validityEndTime = Core.Nothing,
      validityStartTime = Core.Nothing,
      value = Core.Nothing
    }

-- | The fingerprint of the public key.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkFingerprint :: Lens.Lens' PublicKey (Core.Maybe Types.String)
pkFingerprint = Lens.field @"fingerprint"
{-# DEPRECATED pkFingerprint "Use generic-lens or generic-optics with 'fingerprint' instead." #-}

-- | The ending time of validity of the public key.
--
-- /Note:/ Consider using 'validityEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkValidityEndTime :: Lens.Lens' PublicKey (Core.Maybe Core.NominalDiffTime)
pkValidityEndTime = Lens.field @"validityEndTime"
{-# DEPRECATED pkValidityEndTime "Use generic-lens or generic-optics with 'validityEndTime' instead." #-}

-- | The starting time of validity of the public key.
--
-- /Note:/ Consider using 'validityStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkValidityStartTime :: Lens.Lens' PublicKey (Core.Maybe Core.NominalDiffTime)
pkValidityStartTime = Lens.field @"validityStartTime"
{-# DEPRECATED pkValidityStartTime "Use generic-lens or generic-optics with 'validityStartTime' instead." #-}

-- | The DER encoded public key value in PKCS#1 format.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkValue :: Lens.Lens' PublicKey (Core.Maybe Core.Base64)
pkValue = Lens.field @"value"
{-# DEPRECATED pkValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON PublicKey where
  parseJSON =
    Core.withObject "PublicKey" Core.$
      \x ->
        PublicKey'
          Core.<$> (x Core..:? "Fingerprint")
          Core.<*> (x Core..:? "ValidityEndTime")
          Core.<*> (x Core..:? "ValidityStartTime")
          Core.<*> (x Core..:? "Value")
