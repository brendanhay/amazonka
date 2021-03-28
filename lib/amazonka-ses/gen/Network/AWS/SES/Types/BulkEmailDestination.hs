{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.BulkEmailDestination
  ( BulkEmailDestination (..)
  -- * Smart constructor
  , mkBulkEmailDestination
  -- * Lenses
  , bedDestination
  , bedReplacementTags
  , bedReplacementTemplateData
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Destination as Types
import qualified Network.AWS.SES.Types.MessageTag as Types
import qualified Network.AWS.SES.Types.ReplacementTemplateData as Types

-- | An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.
--
-- /See:/ 'mkBulkEmailDestination' smart constructor.
data BulkEmailDestination = BulkEmailDestination'
  { destination :: Types.Destination
  , replacementTags :: Core.Maybe [Types.MessageTag]
    -- ^ A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
  , replacementTemplateData :: Core.Maybe Types.ReplacementTemplateData
    -- ^ A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BulkEmailDestination' value with any optional fields omitted.
mkBulkEmailDestination
    :: Types.Destination -- ^ 'destination'
    -> BulkEmailDestination
mkBulkEmailDestination destination
  = BulkEmailDestination'{destination,
                          replacementTags = Core.Nothing,
                          replacementTemplateData = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedDestination :: Lens.Lens' BulkEmailDestination Types.Destination
bedDestination = Lens.field @"destination"
{-# INLINEABLE bedDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- /Note:/ Consider using 'replacementTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedReplacementTags :: Lens.Lens' BulkEmailDestination (Core.Maybe [Types.MessageTag])
bedReplacementTags = Lens.field @"replacementTags"
{-# INLINEABLE bedReplacementTags #-}
{-# DEPRECATED replacementTags "Use generic-lens or generic-optics with 'replacementTags' instead"  #-}

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- /Note:/ Consider using 'replacementTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bedReplacementTemplateData :: Lens.Lens' BulkEmailDestination (Core.Maybe Types.ReplacementTemplateData)
bedReplacementTemplateData = Lens.field @"replacementTemplateData"
{-# INLINEABLE bedReplacementTemplateData #-}
{-# DEPRECATED replacementTemplateData "Use generic-lens or generic-optics with 'replacementTemplateData' instead"  #-}

instance Core.ToQuery BulkEmailDestination where
        toQuery BulkEmailDestination{..}
          = Core.toQueryPair "Destination" destination Core.<>
              Core.toQueryPair "ReplacementTags"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   replacementTags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplacementTemplateData")
                replacementTemplateData
