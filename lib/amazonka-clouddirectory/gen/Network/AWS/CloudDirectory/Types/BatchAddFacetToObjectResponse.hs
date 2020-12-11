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
import qualified Network.AWS.Prelude as Lude

-- | The result of a batch add facet to object operation.
--
-- /See:/ 'mkBatchAddFacetToObjectResponse' smart constructor.
data BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAddFacetToObjectResponse' with the minimum fields required to make a request.
mkBatchAddFacetToObjectResponse ::
  BatchAddFacetToObjectResponse
mkBatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'

instance Lude.FromJSON BatchAddFacetToObjectResponse where
  parseJSON =
    Lude.withObject
      "BatchAddFacetToObjectResponse"
      (\x -> Lude.pure BatchAddFacetToObjectResponse')
