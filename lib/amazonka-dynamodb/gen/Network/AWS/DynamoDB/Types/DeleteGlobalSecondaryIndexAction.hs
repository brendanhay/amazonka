{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
  ( DeleteGlobalSecondaryIndexAction (..)
  -- * Smart constructor
  , mkDeleteGlobalSecondaryIndexAction
  -- * Lenses
  , dgsiaIndexName
  ) where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a global secondary index to be deleted from an existing table.
--
-- /See:/ 'mkDeleteGlobalSecondaryIndexAction' smart constructor.
newtype DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
  { indexName :: Types.IndexName
    -- ^ The name of the global secondary index to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalSecondaryIndexAction' value with any optional fields omitted.
mkDeleteGlobalSecondaryIndexAction
    :: Types.IndexName -- ^ 'indexName'
    -> DeleteGlobalSecondaryIndexAction
mkDeleteGlobalSecondaryIndexAction indexName
  = DeleteGlobalSecondaryIndexAction'{indexName}

-- | The name of the global secondary index to be deleted.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiaIndexName :: Lens.Lens' DeleteGlobalSecondaryIndexAction Types.IndexName
dgsiaIndexName = Lens.field @"indexName"
{-# INLINEABLE dgsiaIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

instance Core.FromJSON DeleteGlobalSecondaryIndexAction where
        toJSON DeleteGlobalSecondaryIndexAction{..}
          = Core.object
              (Core.catMaybes [Core.Just ("IndexName" Core..= indexName)])
