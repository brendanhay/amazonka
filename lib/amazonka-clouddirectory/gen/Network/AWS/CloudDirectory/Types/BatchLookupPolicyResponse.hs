{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
  ( BatchLookupPolicyResponse (..)
  -- * Smart constructor
  , mkBatchLookupPolicyResponse
  -- * Lenses
  , blprNextToken
  , blprPolicyToPathList
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.PolicyToPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'LookupPolicy' response operation.
--
-- /See:/ 'mkBatchLookupPolicyResponse' smart constructor.
data BatchLookupPolicyResponse = BatchLookupPolicyResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , policyToPathList :: Core.Maybe [Types.PolicyToPath]
    -- ^ Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchLookupPolicyResponse' value with any optional fields omitted.
mkBatchLookupPolicyResponse
    :: BatchLookupPolicyResponse
mkBatchLookupPolicyResponse
  = BatchLookupPolicyResponse'{nextToken = Core.Nothing,
                               policyToPathList = Core.Nothing}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blprNextToken :: Lens.Lens' BatchLookupPolicyResponse (Core.Maybe Types.NextToken)
blprNextToken = Lens.field @"nextToken"
{-# INLINEABLE blprNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'policyToPathList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blprPolicyToPathList :: Lens.Lens' BatchLookupPolicyResponse (Core.Maybe [Types.PolicyToPath])
blprPolicyToPathList = Lens.field @"policyToPathList"
{-# INLINEABLE blprPolicyToPathList #-}
{-# DEPRECATED policyToPathList "Use generic-lens or generic-optics with 'policyToPathList' instead"  #-}

instance Core.FromJSON BatchLookupPolicyResponse where
        parseJSON
          = Core.withObject "BatchLookupPolicyResponse" Core.$
              \ x ->
                BatchLookupPolicyResponse' Core.<$>
                  (x Core..:? "NextToken") Core.<*> x Core..:? "PolicyToPathList"
