{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
  ( Phase2EncryptionAlgorithmsListValue (..),

    -- * Smart constructor
    mkPhase2EncryptionAlgorithmsListValue,

    -- * Lenses
    pealvValue,
  )
where

import qualified Network.AWS.EC2.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption algorithm for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2EncryptionAlgorithmsListValue' smart constructor.
newtype Phase2EncryptionAlgorithmsListValue = Phase2EncryptionAlgorithmsListValue'
  { -- | The encryption algorithm.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase2EncryptionAlgorithmsListValue' value with any optional fields omitted.
mkPhase2EncryptionAlgorithmsListValue ::
  Phase2EncryptionAlgorithmsListValue
mkPhase2EncryptionAlgorithmsListValue =
  Phase2EncryptionAlgorithmsListValue' {value = Core.Nothing}

-- | The encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pealvValue :: Lens.Lens' Phase2EncryptionAlgorithmsListValue (Core.Maybe Types.Value)
pealvValue = Lens.field @"value"
{-# DEPRECATED pealvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML Phase2EncryptionAlgorithmsListValue where
  parseXML x =
    Phase2EncryptionAlgorithmsListValue'
      Core.<$> (x Core..@? "value")
