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
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'DetachTypedLink' response operation.
--
-- /See:/ 'mkBatchDetachTypedLinkResponse' smart constructor.
data BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachTypedLinkResponse' with the minimum fields required to make a request.
mkBatchDetachTypedLinkResponse ::
  BatchDetachTypedLinkResponse
mkBatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'

instance Lude.FromJSON BatchDetachTypedLinkResponse where
  parseJSON =
    Lude.withObject
      "BatchDetachTypedLinkResponse"
      (\x -> Lude.pure BatchDetachTypedLinkResponse')
