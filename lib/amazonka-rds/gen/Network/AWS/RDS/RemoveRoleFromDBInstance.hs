{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveRoleFromDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an AWS Identity and Access Management (IAM) role from a DB instance.
module Network.AWS.RDS.RemoveRoleFromDBInstance
  ( -- * Creating a request
    RemoveRoleFromDBInstance (..),
    mkRemoveRoleFromDBInstance,

    -- ** Request lenses
    rrfdbiDBInstanceIdentifier,
    rrfdbiRoleArn,
    rrfdbiFeatureName,

    -- * Destructuring the response
    RemoveRoleFromDBInstanceResponse (..),
    mkRemoveRoleFromDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveRoleFromDBInstance' smart constructor.
data RemoveRoleFromDBInstance = RemoveRoleFromDBInstance'
  { -- | The name of the DB instance to disassociate the IAM role from.
    dBInstanceIdentifier :: Types.String,
    -- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
    roleArn :: Types.String,
    -- | The name of the feature for the DB instance that the IAM role is to be disassociated from. For the list of supported feature names, see @DBEngineVersion@ .
    featureName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromDBInstance' value with any optional fields omitted.
mkRemoveRoleFromDBInstance ::
  -- | 'dBInstanceIdentifier'
  Types.String ->
  -- | 'roleArn'
  Types.String ->
  -- | 'featureName'
  Types.String ->
  RemoveRoleFromDBInstance
mkRemoveRoleFromDBInstance dBInstanceIdentifier roleArn featureName =
  RemoveRoleFromDBInstance'
    { dBInstanceIdentifier,
      roleArn,
      featureName
    }

-- | The name of the DB instance to disassociate the IAM role from.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbiDBInstanceIdentifier :: Lens.Lens' RemoveRoleFromDBInstance Types.String
rrfdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED rrfdbiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbiRoleArn :: Lens.Lens' RemoveRoleFromDBInstance Types.String
rrfdbiRoleArn = Lens.field @"roleArn"
{-# DEPRECATED rrfdbiRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The name of the feature for the DB instance that the IAM role is to be disassociated from. For the list of supported feature names, see @DBEngineVersion@ .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbiFeatureName :: Lens.Lens' RemoveRoleFromDBInstance Types.String
rrfdbiFeatureName = Lens.field @"featureName"
{-# DEPRECATED rrfdbiFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

instance Core.AWSRequest RemoveRoleFromDBInstance where
  type Rs RemoveRoleFromDBInstance = RemoveRoleFromDBInstanceResponse
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
            ( Core.pure ("Action", "RemoveRoleFromDBInstance")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> (Core.toQueryValue "RoleArn" roleArn)
                Core.<> (Core.toQueryValue "FeatureName" featureName)
            )
      }
  response = Response.receiveNull RemoveRoleFromDBInstanceResponse'

-- | /See:/ 'mkRemoveRoleFromDBInstanceResponse' smart constructor.
data RemoveRoleFromDBInstanceResponse = RemoveRoleFromDBInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromDBInstanceResponse' value with any optional fields omitted.
mkRemoveRoleFromDBInstanceResponse ::
  RemoveRoleFromDBInstanceResponse
mkRemoveRoleFromDBInstanceResponse =
  RemoveRoleFromDBInstanceResponse'
