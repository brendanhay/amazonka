{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
  ( Phase2EncryptionAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase2EncryptionAlgorithmsRequestListValue,

    -- * Lenses
    pearlvfValue,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the encryption algorithm for the VPN tunnel for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2EncryptionAlgorithmsRequestListValue' smart constructor.
newtype Phase2EncryptionAlgorithmsRequestListValue = Phase2EncryptionAlgorithmsRequestListValue'
  { -- | The encryption algorithm.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase2EncryptionAlgorithmsRequestListValue' value with any optional fields omitted.
mkPhase2EncryptionAlgorithmsRequestListValue ::
  Phase2EncryptionAlgorithmsRequestListValue
mkPhase2EncryptionAlgorithmsRequestListValue =
  Phase2EncryptionAlgorithmsRequestListValue' {value = Core.Nothing}

-- | The encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pearlvfValue :: Lens.Lens' Phase2EncryptionAlgorithmsRequestListValue (Core.Maybe Types.String)
pearlvfValue = Lens.field @"value"
{-# DEPRECATED pearlvfValue "Use generic-lens or generic-optics with 'value' instead." #-}
