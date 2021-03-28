{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse
  ( BatchDetachPolicyResponse (..)
  -- * Smart constructor
  , mkBatchDetachPolicyResponse
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DetachPolicy' response operation.
--
-- /See:/ 'mkBatchDetachPolicyResponse' smart constructor.
data BatchDetachPolicyResponse = BatchDetachPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachPolicyResponse' value with any optional fields omitted.
mkBatchDetachPolicyResponse
    :: BatchDetachPolicyResponse
mkBatchDetachPolicyResponse = BatchDetachPolicyResponse'

instance Core.FromJSON BatchDetachPolicyResponse where
        parseJSON
          = Core.withObject "BatchDetachPolicyResponse" Core.$
              \ x -> Core.pure BatchDetachPolicyResponse'
