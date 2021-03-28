{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
  ( Phase1EncryptionAlgorithmsListValue (..)
  -- * Smart constructor
  , mkPhase1EncryptionAlgorithmsListValue
  -- * Lenses
  , pealvfValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption algorithm for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1EncryptionAlgorithmsListValue' smart constructor.
newtype Phase1EncryptionAlgorithmsListValue = Phase1EncryptionAlgorithmsListValue'
  { value :: Core.Maybe Core.Text
    -- ^ The value for the encryption algorithm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase1EncryptionAlgorithmsListValue' value with any optional fields omitted.
mkPhase1EncryptionAlgorithmsListValue
    :: Phase1EncryptionAlgorithmsListValue
mkPhase1EncryptionAlgorithmsListValue
  = Phase1EncryptionAlgorithmsListValue'{value = Core.Nothing}

-- | The value for the encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pealvfValue :: Lens.Lens' Phase1EncryptionAlgorithmsListValue (Core.Maybe Core.Text)
pealvfValue = Lens.field @"value"
{-# INLINEABLE pealvfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML Phase1EncryptionAlgorithmsListValue where
        parseXML x
          = Phase1EncryptionAlgorithmsListValue' Core.<$>
              (x Core..@? "value")
