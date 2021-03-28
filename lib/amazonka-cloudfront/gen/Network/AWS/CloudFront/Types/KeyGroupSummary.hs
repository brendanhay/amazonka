{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KeyGroupSummary
  ( KeyGroupSummary (..)
  -- * Smart constructor
  , mkKeyGroupSummary
  -- * Lenses
  , kgsKeyGroup
  ) where

import qualified Network.AWS.CloudFront.Types.KeyGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a key group.
--
-- /See:/ 'mkKeyGroupSummary' smart constructor.
newtype KeyGroupSummary = KeyGroupSummary'
  { keyGroup :: Types.KeyGroup
    -- ^ A key group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'KeyGroupSummary' value with any optional fields omitted.
mkKeyGroupSummary
    :: Types.KeyGroup -- ^ 'keyGroup'
    -> KeyGroupSummary
mkKeyGroupSummary keyGroup = KeyGroupSummary'{keyGroup}

-- | A key group.
--
-- /Note:/ Consider using 'keyGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgsKeyGroup :: Lens.Lens' KeyGroupSummary Types.KeyGroup
kgsKeyGroup = Lens.field @"keyGroup"
{-# INLINEABLE kgsKeyGroup #-}
{-# DEPRECATED keyGroup "Use generic-lens or generic-optics with 'keyGroup' instead"  #-}

instance Core.FromXML KeyGroupSummary where
        parseXML x = KeyGroupSummary' Core.<$> (x Core..@ "KeyGroup")
