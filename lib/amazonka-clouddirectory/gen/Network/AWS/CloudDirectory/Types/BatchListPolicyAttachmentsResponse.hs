{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
  ( BatchListPolicyAttachmentsResponse (..)
  -- * Smart constructor
  , mkBatchListPolicyAttachmentsResponse
  -- * Lenses
  , blparNextToken
  , blparObjectIdentifiers
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListPolicyAttachments' response operation.
--
-- /See:/ 'mkBatchListPolicyAttachmentsResponse' smart constructor.
data BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , objectIdentifiers :: Core.Maybe [Types.ObjectIdentifier]
    -- ^ A list of @ObjectIdentifiers@ to which the policy is attached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListPolicyAttachmentsResponse' value with any optional fields omitted.
mkBatchListPolicyAttachmentsResponse
    :: BatchListPolicyAttachmentsResponse
mkBatchListPolicyAttachmentsResponse
  = BatchListPolicyAttachmentsResponse'{nextToken = Core.Nothing,
                                        objectIdentifiers = Core.Nothing}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blparNextToken :: Lens.Lens' BatchListPolicyAttachmentsResponse (Core.Maybe Types.NextToken)
blparNextToken = Lens.field @"nextToken"
{-# INLINEABLE blparNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blparObjectIdentifiers :: Lens.Lens' BatchListPolicyAttachmentsResponse (Core.Maybe [Types.ObjectIdentifier])
blparObjectIdentifiers = Lens.field @"objectIdentifiers"
{-# INLINEABLE blparObjectIdentifiers #-}
{-# DEPRECATED objectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead"  #-}

instance Core.FromJSON BatchListPolicyAttachmentsResponse where
        parseJSON
          = Core.withObject "BatchListPolicyAttachmentsResponse" Core.$
              \ x ->
                BatchListPolicyAttachmentsResponse' Core.<$>
                  (x Core..:? "NextToken") Core.<*> x Core..:? "ObjectIdentifiers"
