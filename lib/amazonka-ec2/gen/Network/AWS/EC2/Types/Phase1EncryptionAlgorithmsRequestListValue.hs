{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
  ( Phase1EncryptionAlgorithmsRequestListValue (..)
  -- * Smart constructor
  , mkPhase1EncryptionAlgorithmsRequestListValue
  -- * Lenses
  , pearlvValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the encryption algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1EncryptionAlgorithmsRequestListValue' smart constructor.
newtype Phase1EncryptionAlgorithmsRequestListValue = Phase1EncryptionAlgorithmsRequestListValue'
  { value :: Core.Maybe Core.Text
    -- ^ The value for the encryption algorithm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Phase1EncryptionAlgorithmsRequestListValue' value with any optional fields omitted.
mkPhase1EncryptionAlgorithmsRequestListValue
    :: Phase1EncryptionAlgorithmsRequestListValue
mkPhase1EncryptionAlgorithmsRequestListValue
  = Phase1EncryptionAlgorithmsRequestListValue'{value = Core.Nothing}

-- | The value for the encryption algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pearlvValue :: Lens.Lens' Phase1EncryptionAlgorithmsRequestListValue (Core.Maybe Core.Text)
pearlvValue = Lens.field @"value"
{-# INLINEABLE pearlvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery Phase1EncryptionAlgorithmsRequestListValue
         where
        toQuery Phase1EncryptionAlgorithmsRequestListValue{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Value") value
