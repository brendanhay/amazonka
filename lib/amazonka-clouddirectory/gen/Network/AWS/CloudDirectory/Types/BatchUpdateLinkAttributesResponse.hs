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
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'UpdateLinkAttributes' response operation.
--
-- /See:/ 'mkBatchUpdateLinkAttributesResponse' smart constructor.
data BatchUpdateLinkAttributesResponse = BatchUpdateLinkAttributesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdateLinkAttributesResponse' with the minimum fields required to make a request.
mkBatchUpdateLinkAttributesResponse ::
  BatchUpdateLinkAttributesResponse
mkBatchUpdateLinkAttributesResponse =
  BatchUpdateLinkAttributesResponse'

instance Lude.FromJSON BatchUpdateLinkAttributesResponse where
  parseJSON =
    Lude.withObject
      "BatchUpdateLinkAttributesResponse"
      (\x -> Lude.pure BatchUpdateLinkAttributesResponse')
