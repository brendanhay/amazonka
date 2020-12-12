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
import qualified Network.AWS.Prelude as Lude

-- | An empty result that represents success.
--
-- /See:/ 'mkBatchRemoveFacetFromObjectResponse' smart constructor.
data BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchRemoveFacetFromObjectResponse' with the minimum fields required to make a request.
mkBatchRemoveFacetFromObjectResponse ::
  BatchRemoveFacetFromObjectResponse
mkBatchRemoveFacetFromObjectResponse =
  BatchRemoveFacetFromObjectResponse'

instance Lude.FromJSON BatchRemoveFacetFromObjectResponse where
  parseJSON =
    Lude.withObject
      "BatchRemoveFacetFromObjectResponse"
      (\x -> Lude.pure BatchRemoveFacetFromObjectResponse')
