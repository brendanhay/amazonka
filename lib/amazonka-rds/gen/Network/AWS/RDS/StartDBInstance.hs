{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon RDS DB instance that was stopped using the AWS console, the stop-db-instance AWS CLI command, or the StopDBInstance action.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StartInstance.html Starting an Amazon RDS DB instance That Was Previously Stopped> in the /Amazon RDS User Guide./
module Network.AWS.RDS.StartDBInstance
  ( -- * Creating a request
    StartDBInstance (..),
    mkStartDBInstance,

    -- ** Request lenses
    sDBInstanceIdentifier,

    -- * Destructuring the response
    StartDBInstanceResponse (..),
    mkStartDBInstanceResponse,

    -- ** Response lenses
    sdbirfrsDBInstance,
    sdbirfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDBInstance' smart constructor.
newtype StartDBInstance = StartDBInstance'
  { -- | The user-supplied instance identifier.
    dBInstanceIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartDBInstance' value with any optional fields omitted.
mkStartDBInstance ::
  -- | 'dBInstanceIdentifier'
  Types.String ->
  StartDBInstance
mkStartDBInstance dBInstanceIdentifier =
  StartDBInstance' {dBInstanceIdentifier}

-- | The user-supplied instance identifier.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDBInstanceIdentifier :: Lens.Lens' StartDBInstance Types.String
sDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED sDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

instance Core.AWSRequest StartDBInstance where
  type Rs StartDBInstance = StartDBInstanceResponse
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
            ( Core.pure ("Action", "StartDBInstance")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "StartDBInstanceResult"
      ( \s h x ->
          StartDBInstanceResponse'
            Core.<$> (x Core..@? "DBInstance") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartDBInstanceResponse' smart constructor.
data StartDBInstanceResponse = StartDBInstanceResponse'
  { dBInstance :: Core.Maybe Types.DBInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartDBInstanceResponse' value with any optional fields omitted.
mkStartDBInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartDBInstanceResponse
mkStartDBInstanceResponse responseStatus =
  StartDBInstanceResponse'
    { dBInstance = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirfrsDBInstance :: Lens.Lens' StartDBInstanceResponse (Core.Maybe Types.DBInstance)
sdbirfrsDBInstance = Lens.field @"dBInstance"
{-# DEPRECATED sdbirfrsDBInstance "Use generic-lens or generic-optics with 'dBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirfrsResponseStatus :: Lens.Lens' StartDBInstanceResponse Core.Int
sdbirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sdbirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
