{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ResetDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group to the default value. To reset specific parameters submit a list of the following: @ParameterName@ and @ApplyMethod@ . To reset the entire DB cluster parameter group, specify the @DBClusterParameterGroupName@ and @ResetAllParameters@ parameters.
--
-- When resetting the entire group, dynamic parameters are updated immediately and static parameters are set to @pending-reboot@ to take effect on the next DB instance restart or @RebootDBInstance@ request. You must call @RebootDBInstance@ for every DB instance in your DB cluster that you want the updated static parameter to apply to.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.ResetDBClusterParameterGroup
  ( -- * Creating a request
    ResetDBClusterParameterGroup (..),
    mkResetDBClusterParameterGroup,

    -- ** Request lenses
    rdbcpgDBClusterParameterGroupName,
    rdbcpgParameters,
    rdbcpgResetAllParameters,

    -- * Destructuring the response
    Types.DBClusterParameterGroupNameMessage (..),
    Types.mkDBClusterParameterGroupNameMessage,

    -- ** Response lenses
    Types.dbcpgnmDBClusterParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkResetDBClusterParameterGroup' smart constructor.
data ResetDBClusterParameterGroup = ResetDBClusterParameterGroup'
  { -- | The name of the DB cluster parameter group to reset.
    dBClusterParameterGroupName :: Types.DBClusterParameterGroupName,
    -- | A list of parameter names in the DB cluster parameter group to reset to the default values. You can't use this parameter if the @ResetAllParameters@ parameter is enabled.
    parameters :: Core.Maybe [Types.Parameter],
    -- | A value that indicates whether to reset all parameters in the DB cluster parameter group to their default values. You can't use this parameter if there is a list of parameter names specified for the @Parameters@ parameter.
    resetAllParameters :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetDBClusterParameterGroup' value with any optional fields omitted.
mkResetDBClusterParameterGroup ::
  -- | 'dBClusterParameterGroupName'
  Types.DBClusterParameterGroupName ->
  ResetDBClusterParameterGroup
mkResetDBClusterParameterGroup dBClusterParameterGroupName =
  ResetDBClusterParameterGroup'
    { dBClusterParameterGroupName,
      parameters = Core.Nothing,
      resetAllParameters = Core.Nothing
    }

-- | The name of the DB cluster parameter group to reset.
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcpgDBClusterParameterGroupName :: Lens.Lens' ResetDBClusterParameterGroup Types.DBClusterParameterGroupName
rdbcpgDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED rdbcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | A list of parameter names in the DB cluster parameter group to reset to the default values. You can't use this parameter if the @ResetAllParameters@ parameter is enabled.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcpgParameters :: Lens.Lens' ResetDBClusterParameterGroup (Core.Maybe [Types.Parameter])
rdbcpgParameters = Lens.field @"parameters"
{-# DEPRECATED rdbcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A value that indicates whether to reset all parameters in the DB cluster parameter group to their default values. You can't use this parameter if there is a list of parameter names specified for the @Parameters@ parameter.
--
-- /Note:/ Consider using 'resetAllParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbcpgResetAllParameters :: Lens.Lens' ResetDBClusterParameterGroup (Core.Maybe Core.Bool)
rdbcpgResetAllParameters = Lens.field @"resetAllParameters"
{-# DEPRECATED rdbcpgResetAllParameters "Use generic-lens or generic-optics with 'resetAllParameters' instead." #-}

instance Core.AWSRequest ResetDBClusterParameterGroup where
  type
    Rs ResetDBClusterParameterGroup =
      Types.DBClusterParameterGroupNameMessage
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
            ( Core.pure ("Action", "ResetDBClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterParameterGroupName"
                            dBClusterParameterGroupName
                        )
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
      "ResetDBClusterParameterGroupResult"
      (\s h x -> Core.parseXML x)
