{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
  ( BatchListObjectPoliciesResponse (..)
  -- * Smart constructor
  , mkBatchListObjectPoliciesResponse
  -- * Lenses
  , bAttachedPolicyIds
  , bNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListObjectPolicies' response operation.
--
-- /See:/ 'mkBatchListObjectPoliciesResponse' smart constructor.
data BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse'
  { attachedPolicyIds :: Core.Maybe [Types.ObjectIdentifier]
    -- ^ A list of policy @ObjectIdentifiers@ , that are attached to the object.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectPoliciesResponse' value with any optional fields omitted.
mkBatchListObjectPoliciesResponse
    :: BatchListObjectPoliciesResponse
mkBatchListObjectPoliciesResponse
  = BatchListObjectPoliciesResponse'{attachedPolicyIds =
                                       Core.Nothing,
                                     nextToken = Core.Nothing}

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
--
-- /Note:/ Consider using 'attachedPolicyIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bAttachedPolicyIds :: Lens.Lens' BatchListObjectPoliciesResponse (Core.Maybe [Types.ObjectIdentifier])
bAttachedPolicyIds = Lens.field @"attachedPolicyIds"
{-# INLINEABLE bAttachedPolicyIds #-}
{-# DEPRECATED attachedPolicyIds "Use generic-lens or generic-optics with 'attachedPolicyIds' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNextToken :: Lens.Lens' BatchListObjectPoliciesResponse (Core.Maybe Types.NextToken)
bNextToken = Lens.field @"nextToken"
{-# INLINEABLE bNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListObjectPoliciesResponse where
        parseJSON
          = Core.withObject "BatchListObjectPoliciesResponse" Core.$
              \ x ->
                BatchListObjectPoliciesResponse' Core.<$>
                  (x Core..:? "AttachedPolicyIds") Core.<*> x Core..:? "NextToken"
