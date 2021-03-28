{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
  ( BatchListPolicyAttachments (..)
  -- * Smart constructor
  , mkBatchListPolicyAttachments
  -- * Lenses
  , blpaPolicyReference
  , blpaMaxResults
  , blpaNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached inside a 'BatchRead' operation. For more information, see 'ListPolicyAttachments' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListPolicyAttachments' smart constructor.
data BatchListPolicyAttachments = BatchListPolicyAttachments'
  { policyReference :: Types.ObjectReference
    -- ^ The reference that identifies the policy object.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListPolicyAttachments' value with any optional fields omitted.
mkBatchListPolicyAttachments
    :: Types.ObjectReference -- ^ 'policyReference'
    -> BatchListPolicyAttachments
mkBatchListPolicyAttachments policyReference
  = BatchListPolicyAttachments'{policyReference,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpaPolicyReference :: Lens.Lens' BatchListPolicyAttachments Types.ObjectReference
blpaPolicyReference = Lens.field @"policyReference"
{-# INLINEABLE blpaPolicyReference #-}
{-# DEPRECATED policyReference "Use generic-lens or generic-optics with 'policyReference' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpaMaxResults :: Lens.Lens' BatchListPolicyAttachments (Core.Maybe Core.Natural)
blpaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE blpaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpaNextToken :: Lens.Lens' BatchListPolicyAttachments (Core.Maybe Types.NextToken)
blpaNextToken = Lens.field @"nextToken"
{-# INLINEABLE blpaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListPolicyAttachments where
        toJSON BatchListPolicyAttachments{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyReference" Core..= policyReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
