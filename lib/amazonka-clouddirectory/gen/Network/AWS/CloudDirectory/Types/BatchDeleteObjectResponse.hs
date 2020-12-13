{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse
  ( BatchDeleteObjectResponse (..),

    -- * Smart constructor
    mkBatchDeleteObjectResponse,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'DeleteObject' response operation.
--
-- /See:/ 'mkBatchDeleteObjectResponse' smart constructor.
data BatchDeleteObjectResponse = BatchDeleteObjectResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteObjectResponse' with the minimum fields required to make a request.
mkBatchDeleteObjectResponse ::
  BatchDeleteObjectResponse
mkBatchDeleteObjectResponse = BatchDeleteObjectResponse'

instance Lude.FromJSON BatchDeleteObjectResponse where
  parseJSON =
    Lude.withObject
      "BatchDeleteObjectResponse"
      (\x -> Lude.pure BatchDeleteObjectResponse')
