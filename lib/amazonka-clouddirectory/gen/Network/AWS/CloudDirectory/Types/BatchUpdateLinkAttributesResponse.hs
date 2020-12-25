{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
  ( BatchUpdateLinkAttributesResponse (..),

    -- * Smart constructor
    mkBatchUpdateLinkAttributesResponse,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'UpdateLinkAttributes' response operation.
--
-- /See:/ 'mkBatchUpdateLinkAttributesResponse' smart constructor.
data BatchUpdateLinkAttributesResponse = BatchUpdateLinkAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchUpdateLinkAttributesResponse' value with any optional fields omitted.
mkBatchUpdateLinkAttributesResponse ::
  BatchUpdateLinkAttributesResponse
mkBatchUpdateLinkAttributesResponse =
  BatchUpdateLinkAttributesResponse'

instance Core.FromJSON BatchUpdateLinkAttributesResponse where
  parseJSON =
    Core.withObject "BatchUpdateLinkAttributesResponse" Core.$
      \x -> Core.pure BatchUpdateLinkAttributesResponse'
