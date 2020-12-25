{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse
  ( BatchAttachPolicyResponse (..),

    -- * Smart constructor
    mkBatchAttachPolicyResponse,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of an 'AttachPolicy' response operation.
--
-- /See:/ 'mkBatchAttachPolicyResponse' smart constructor.
data BatchAttachPolicyResponse = BatchAttachPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAttachPolicyResponse' value with any optional fields omitted.
mkBatchAttachPolicyResponse ::
  BatchAttachPolicyResponse
mkBatchAttachPolicyResponse = BatchAttachPolicyResponse'

instance Core.FromJSON BatchAttachPolicyResponse where
  parseJSON =
    Core.withObject "BatchAttachPolicyResponse" Core.$
      \x -> Core.pure BatchAttachPolicyResponse'
