{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
  ( UpdateGlobalSecondaryIndexAction (..)
  -- * Smart constructor
  , mkUpdateGlobalSecondaryIndexAction
  -- * Lenses
  , ugsiaIndexName
  , ugsiaProvisionedThroughput
  ) where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the new provisioned throughput settings to be applied to a global secondary index.
--
-- /See:/ 'mkUpdateGlobalSecondaryIndexAction' smart constructor.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction'
  { indexName :: Types.IndexName
    -- ^ The name of the global secondary index to be updated.
  , provisionedThroughput :: Types.ProvisionedThroughput
    -- ^ Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGlobalSecondaryIndexAction' value with any optional fields omitted.
mkUpdateGlobalSecondaryIndexAction
    :: Types.IndexName -- ^ 'indexName'
    -> Types.ProvisionedThroughput -- ^ 'provisionedThroughput'
    -> UpdateGlobalSecondaryIndexAction
mkUpdateGlobalSecondaryIndexAction indexName provisionedThroughput
  = UpdateGlobalSecondaryIndexAction'{indexName,
                                      provisionedThroughput}

-- | The name of the global secondary index to be updated.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsiaIndexName :: Lens.Lens' UpdateGlobalSecondaryIndexAction Types.IndexName
ugsiaIndexName = Lens.field @"indexName"
{-# INLINEABLE ugsiaIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsiaProvisionedThroughput :: Lens.Lens' UpdateGlobalSecondaryIndexAction Types.ProvisionedThroughput
ugsiaProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# INLINEABLE ugsiaProvisionedThroughput #-}
{-# DEPRECATED provisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead"  #-}

instance Core.FromJSON UpdateGlobalSecondaryIndexAction where
        toJSON UpdateGlobalSecondaryIndexAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexName" Core..= indexName),
                  Core.Just ("ProvisionedThroughput" Core..= provisionedThroughput)])
