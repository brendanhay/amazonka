{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddRoleToDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Identity and Access Management (IAM) role with a DB instance.
module Network.AWS.RDS.AddRoleToDBInstance
    (
    -- * Creating a request
      AddRoleToDBInstance (..)
    , mkAddRoleToDBInstance
    -- ** Request lenses
    , artdbiDBInstanceIdentifier
    , artdbiRoleArn
    , artdbiFeatureName

    -- * Destructuring the response
    , AddRoleToDBInstanceResponse (..)
    , mkAddRoleToDBInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddRoleToDBInstance' smart constructor.
data AddRoleToDBInstance = AddRoleToDBInstance'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The name of the DB instance to associate the IAM role with.
  , roleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ . 
  , featureName :: Core.Text
    -- ^ The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToDBInstance' value with any optional fields omitted.
mkAddRoleToDBInstance
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> Core.Text -- ^ 'roleArn'
    -> Core.Text -- ^ 'featureName'
    -> AddRoleToDBInstance
mkAddRoleToDBInstance dBInstanceIdentifier roleArn featureName
  = AddRoleToDBInstance'{dBInstanceIdentifier, roleArn, featureName}

-- | The name of the DB instance to associate the IAM role with.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbiDBInstanceIdentifier :: Lens.Lens' AddRoleToDBInstance Core.Text
artdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE artdbiDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ . 
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbiRoleArn :: Lens.Lens' AddRoleToDBInstance Core.Text
artdbiRoleArn = Lens.field @"roleArn"
{-# INLINEABLE artdbiRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' . 
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbiFeatureName :: Lens.Lens' AddRoleToDBInstance Core.Text
artdbiFeatureName = Lens.field @"featureName"
{-# INLINEABLE artdbiFeatureName #-}
{-# DEPRECATED featureName "Use generic-lens or generic-optics with 'featureName' instead"  #-}

instance Core.ToQuery AddRoleToDBInstance where
        toQuery AddRoleToDBInstance{..}
          = Core.toQueryPair "Action" ("AddRoleToDBInstance" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<> Core.toQueryPair "RoleArn" roleArn
              Core.<> Core.toQueryPair "FeatureName" featureName

instance Core.ToHeaders AddRoleToDBInstance where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddRoleToDBInstance where
        type Rs AddRoleToDBInstance = AddRoleToDBInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AddRoleToDBInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddRoleToDBInstanceResponse' smart constructor.
data AddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToDBInstanceResponse' value with any optional fields omitted.
mkAddRoleToDBInstanceResponse
    :: AddRoleToDBInstanceResponse
mkAddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
