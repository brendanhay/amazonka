{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.ChangeBatch
  ( ChangeBatch (..)
  -- * Smart constructor
  , mkChangeBatch
  -- * Lenses
  , cbChanges
  , cbComment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Change as Types
import qualified Network.AWS.Route53.Types.ResourceDescription as Types

-- | The information for a change request.
--
-- /See:/ 'mkChangeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
  { changes :: Core.NonEmpty Types.Change
    -- ^ Information about the changes to make to the record sets.
  , comment :: Core.Maybe Types.ResourceDescription
    -- ^ /Optional:/ Any comments you want to include about a change batch request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeBatch' value with any optional fields omitted.
mkChangeBatch
    :: Core.NonEmpty Types.Change -- ^ 'changes'
    -> ChangeBatch
mkChangeBatch changes
  = ChangeBatch'{changes, comment = Core.Nothing}

-- | Information about the changes to make to the record sets.
--
-- /Note:/ Consider using 'changes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbChanges :: Lens.Lens' ChangeBatch (Core.NonEmpty Types.Change)
cbChanges = Lens.field @"changes"
{-# INLINEABLE cbChanges #-}
{-# DEPRECATED changes "Use generic-lens or generic-optics with 'changes' instead"  #-}

-- | /Optional:/ Any comments you want to include about a change batch request.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbComment :: Lens.Lens' ChangeBatch (Core.Maybe Types.ResourceDescription)
cbComment = Lens.field @"comment"
{-# INLINEABLE cbComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToXML ChangeBatch where
        toXML ChangeBatch{..}
          = Core.toXMLElement "Changes" (Core.toXMLList "Change" changes)
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment
