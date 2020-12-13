{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
  ( BatchLookupPolicyResponse (..),

    -- * Smart constructor
    mkBatchLookupPolicyResponse,

    -- * Lenses
    blpNextToken,
    blpPolicyToPathList,
  )
where

import Network.AWS.CloudDirectory.Types.PolicyToPath
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'LookupPolicy' response operation.
--
-- /See:/ 'mkBatchLookupPolicyResponse' smart constructor.
data BatchLookupPolicyResponse = BatchLookupPolicyResponse'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
    policyToPathList :: Lude.Maybe [PolicyToPath]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchLookupPolicyResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'policyToPathList' - Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
mkBatchLookupPolicyResponse ::
  BatchLookupPolicyResponse
mkBatchLookupPolicyResponse =
  BatchLookupPolicyResponse'
    { nextToken = Lude.Nothing,
      policyToPathList = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpNextToken :: Lens.Lens' BatchLookupPolicyResponse (Lude.Maybe Lude.Text)
blpNextToken = Lens.lens (nextToken :: BatchLookupPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchLookupPolicyResponse)
{-# DEPRECATED blpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'policyToPathList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpPolicyToPathList :: Lens.Lens' BatchLookupPolicyResponse (Lude.Maybe [PolicyToPath])
blpPolicyToPathList = Lens.lens (policyToPathList :: BatchLookupPolicyResponse -> Lude.Maybe [PolicyToPath]) (\s a -> s {policyToPathList = a} :: BatchLookupPolicyResponse)
{-# DEPRECATED blpPolicyToPathList "Use generic-lens or generic-optics with 'policyToPathList' instead." #-}

instance Lude.FromJSON BatchLookupPolicyResponse where
  parseJSON =
    Lude.withObject
      "BatchLookupPolicyResponse"
      ( \x ->
          BatchLookupPolicyResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "PolicyToPathList" Lude..!= Lude.mempty)
      )
