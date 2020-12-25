{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
  ( BatchAddFacetToObjectResponse (..),

    -- * Smart constructor
    mkBatchAddFacetToObjectResponse,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of a batch add facet to object operation.
--
-- /See:/ 'mkBatchAddFacetToObjectResponse' smart constructor.
data BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAddFacetToObjectResponse' value with any optional fields omitted.
mkBatchAddFacetToObjectResponse ::
  BatchAddFacetToObjectResponse
mkBatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'

instance Core.FromJSON BatchAddFacetToObjectResponse where
  parseJSON =
    Core.withObject "BatchAddFacetToObjectResponse" Core.$
      \x -> Core.pure BatchAddFacetToObjectResponse'
