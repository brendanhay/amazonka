{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
  ( BatchDetachTypedLinkResponse (..),

    -- * Smart constructor
    mkBatchDetachTypedLinkResponse,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DetachTypedLink' response operation.
--
-- /See:/ 'mkBatchDetachTypedLinkResponse' smart constructor.
data BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachTypedLinkResponse' value with any optional fields omitted.
mkBatchDetachTypedLinkResponse ::
  BatchDetachTypedLinkResponse
mkBatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'

instance Core.FromJSON BatchDetachTypedLinkResponse where
  parseJSON =
    Core.withObject "BatchDetachTypedLinkResponse" Core.$
      \x -> Core.pure BatchDetachTypedLinkResponse'
