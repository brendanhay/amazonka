{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.PolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.PolicyStatus
  ( PolicyStatus (..)
  -- * Smart constructor
  , mkPolicyStatus
  -- * Lenses
  , psIsPublic
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | The container element for a bucket's policy status.
--
-- /See:/ 'mkPolicyStatus' smart constructor.
newtype PolicyStatus = PolicyStatus'
  { isPublic :: Core.Maybe Core.Bool
    -- ^ The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyStatus' value with any optional fields omitted.
mkPolicyStatus
    :: PolicyStatus
mkPolicyStatus = PolicyStatus'{isPublic = Core.Nothing}

-- | The policy status for this bucket. @TRUE@ indicates that this bucket is public. @FALSE@ indicates that the bucket is not public.
--
-- /Note:/ Consider using 'isPublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psIsPublic :: Lens.Lens' PolicyStatus (Core.Maybe Core.Bool)
psIsPublic = Lens.field @"isPublic"
{-# INLINEABLE psIsPublic #-}
{-# DEPRECATED isPublic "Use generic-lens or generic-optics with 'isPublic' instead"  #-}

instance Core.FromXML PolicyStatus where
        parseXML x = PolicyStatus' Core.<$> (x Core..@? "IsPublic")
