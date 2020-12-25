{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AddRoleToDBInstance (..),
    mkAddRoleToDBInstance,

    -- ** Request lenses
    artdbiDBInstanceIdentifier,
    artdbiRoleArn,
    artdbiFeatureName,

    -- * Destructuring the response
    AddRoleToDBInstanceResponse (..),
    mkAddRoleToDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddRoleToDBInstance' smart constructor.
data AddRoleToDBInstance = AddRoleToDBInstance'
  { -- | The name of the DB instance to associate the IAM role with.
    dBInstanceIdentifier :: Types.DBInstanceIdentifier,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
    roleArn :: Types.RoleArn,
    -- | The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
    featureName :: Types.FeatureName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToDBInstance' value with any optional fields omitted.
mkAddRoleToDBInstance ::
  -- | 'dBInstanceIdentifier'
  Types.DBInstanceIdentifier ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'featureName'
  Types.FeatureName ->
  AddRoleToDBInstance
mkAddRoleToDBInstance dBInstanceIdentifier roleArn featureName =
  AddRoleToDBInstance' {dBInstanceIdentifier, roleArn, featureName}

-- | The name of the DB instance to associate the IAM role with.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbiDBInstanceIdentifier :: Lens.Lens' AddRoleToDBInstance Types.DBInstanceIdentifier
artdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED artdbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbiRoleArn :: Lens.Lens' AddRoleToDBInstance Types.RoleArn
artdbiRoleArn = Lens.field @"roleArn"
{-# DEPRECATED artdbiRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbiFeatureName :: Lens.Lens' AddRoleToDBInstance Types.FeatureName
artdbiFeatureName = Lens.field @"featureName"
{-# DEPRECATED artdbiFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

instance Core.AWSRequest AddRoleToDBInstance where
  type Rs AddRoleToDBInstance = AddRoleToDBInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "AddRoleToDBInstance")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> (Core.toQueryValue "RoleArn" roleArn)
                Core.<> (Core.toQueryValue "FeatureName" featureName)
            )
      }
  response = Response.receiveNull AddRoleToDBInstanceResponse'

-- | /See:/ 'mkAddRoleToDBInstanceResponse' smart constructor.
data AddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToDBInstanceResponse' value with any optional fields omitted.
mkAddRoleToDBInstanceResponse ::
  AddRoleToDBInstanceResponse
mkAddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
