{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
  ( BatchRemoveFacetFromObjectResponse (..),

    -- * Smart constructor
    mkBatchRemoveFacetFromObjectResponse,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An empty result that represents success.
--
-- /See:/ 'mkBatchRemoveFacetFromObjectResponse' smart constructor.
data BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchRemoveFacetFromObjectResponse' value with any optional fields omitted.
mkBatchRemoveFacetFromObjectResponse ::
  BatchRemoveFacetFromObjectResponse
mkBatchRemoveFacetFromObjectResponse =
  BatchRemoveFacetFromObjectResponse'

instance Core.FromJSON BatchRemoveFacetFromObjectResponse where
  parseJSON =
    Core.withObject "BatchRemoveFacetFromObjectResponse" Core.$
      \x -> Core.pure BatchRemoveFacetFromObjectResponse'
