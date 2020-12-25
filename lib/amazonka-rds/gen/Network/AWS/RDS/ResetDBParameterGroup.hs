{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB parameter group to the engine/system default value. To reset specific parameters, provide a list of the following: @ParameterName@ and @ApplyMethod@ . To reset the entire DB parameter group, specify the @DBParameterGroup@ name and @ResetAllParameters@ parameters. When resetting the entire group, dynamic parameters are updated immediately and static parameters are set to @pending-reboot@ to take effect on the next DB instance restart or @RebootDBInstance@ request.
module Network.AWS.RDS.ResetDBParameterGroup
  ( -- * Creating a request
    ResetDBParameterGroup (..),
    mkResetDBParameterGroup,

    -- ** Request lenses
    rdbpgDBParameterGroupName,
    rdbpgParameters,
    rdbpgResetAllParameters,

    -- * Destructuring the response
    Types.DBParameterGroupNameMessage (..),
    Types.mkDBParameterGroupNameMessage,

    -- ** Response lenses
    Types.dbpgnmDBParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkResetDBParameterGroup' smart constructor.
data ResetDBParameterGroup = ResetDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    --     * Must match the name of an existing @DBParameterGroup@ .
    dBParameterGroupName :: Types.String,
    -- | To reset the entire DB parameter group, specify the @DBParameterGroup@ name and @ResetAllParameters@ parameters. To reset specific parameters, provide a list of the following: @ParameterName@ and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
    --
    -- __MySQL__
    -- Valid Values (for Apply method): @immediate@ | @pending-reboot@
    -- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
    -- __MariaDB__
    -- Valid Values (for Apply method): @immediate@ | @pending-reboot@
    -- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
    -- __Oracle__
    -- Valid Values (for Apply method): @pending-reboot@
    parameters :: Core.Maybe [Types.Parameter],
    -- | A value that indicates whether to reset all parameters in the DB parameter group to default values. By default, all parameters in the DB parameter group are reset to default values.
    resetAllParameters :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetDBParameterGroup' value with any optional fields omitted.
mkResetDBParameterGroup ::
  -- | 'dBParameterGroupName'
  Types.String ->
  ResetDBParameterGroup
mkResetDBParameterGroup dBParameterGroupName =
  ResetDBParameterGroup'
    { dBParameterGroupName,
      parameters = Core.Nothing,
      resetAllParameters = Core.Nothing
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing @DBParameterGroup@ .
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbpgDBParameterGroupName :: Lens.Lens' ResetDBParameterGroup Types.String
rdbpgDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED rdbpgDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | To reset the entire DB parameter group, specify the @DBParameterGroup@ name and @ResetAllParameters@ parameters. To reset specific parameters, provide a list of the following: @ParameterName@ and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
-- __MySQL__
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
-- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
-- __MariaDB__
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
-- You can use the immediate value with dynamic parameters only. You can use the @pending-reboot@ value for both dynamic and static parameters, and changes are applied when DB instance reboots.
-- __Oracle__
-- Valid Values (for Apply method): @pending-reboot@
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbpgParameters :: Lens.Lens' ResetDBParameterGroup (Core.Maybe [Types.Parameter])
rdbpgParameters = Lens.field @"parameters"
{-# DEPRECATED rdbpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A value that indicates whether to reset all parameters in the DB parameter group to default values. By default, all parameters in the DB parameter group are reset to default values.
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbpgResetAllParameters :: Lens.Lens' ResetDBParameterGroup (Core.Maybe Core.Bool)
rdbpgResetAllParameters = Lens.field @"resetAllParameters"
{-# DEPRECATED rdbpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

instance Core.AWSRequest ResetDBParameterGroup where
  type Rs ResetDBParameterGroup = Types.DBParameterGroupNameMessage
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
            ( Core.pure ("Action", "ResetDBParameterGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBParameterGroupName" dBParameterGroupName)
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "Parameter" Core.<$> parameters)
                        )
                Core.<> ( Core.toQueryValue "ResetAllParameters"
                            Core.<$> resetAllParameters
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ResetDBParameterGroupResult"
      (\s h x -> Core.parseXML x)
