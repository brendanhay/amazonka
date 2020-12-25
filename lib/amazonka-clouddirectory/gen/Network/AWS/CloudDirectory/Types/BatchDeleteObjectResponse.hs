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
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DeleteObject' response operation.
--
-- /See:/ 'mkBatchDeleteObjectResponse' smart constructor.
data BatchDeleteObjectResponse = BatchDeleteObjectResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteObjectResponse' value with any optional fields omitted.
mkBatchDeleteObjectResponse ::
  BatchDeleteObjectResponse
mkBatchDeleteObjectResponse = BatchDeleteObjectResponse'

instance Core.FromJSON BatchDeleteObjectResponse where
  parseJSON =
    Core.withObject "BatchDeleteObjectResponse" Core.$
      \x -> Core.pure BatchDeleteObjectResponse'
