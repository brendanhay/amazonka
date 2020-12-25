{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group. To modify more than one parameter, submit a list of the following: @ParameterName@ , @ParameterValue@ , and @ApplyMethod@ . A maximum of 20 parameters can be modified in a single request.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
-- /Important:/ After you create a DB cluster parameter group, you should wait at least 5 minutes before creating your first DB cluster that uses that DB cluster parameter group as the default parameter group. This allows Amazon RDS to fully complete the create action before the parameter group is used as the default for a new DB cluster. This is especially important for parameters that are critical when creating the default database for a DB cluster, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the @DescribeDBClusterParameters@ action to verify that your DB cluster parameter group has been created or modified.
-- If the modified DB cluster parameter group is used by an Aurora Serverless cluster, Aurora applies the update immediately. The cluster restart might interrupt your workload. In that case, your application must reopen any connections and retry any transactions that were active when the parameter changes took effect.
module Network.AWS.RDS.ModifyDBClusterParameterGroup
  ( -- * Creating a request
    ModifyDBClusterParameterGroup (..),
    mkModifyDBClusterParameterGroup,

    -- ** Request lenses
    mdbcpgDBClusterParameterGroupName,
    mdbcpgParameters,

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
-- /See:/ 'mkModifyDBClusterParameterGroup' smart constructor.
data ModifyDBClusterParameterGroup = ModifyDBClusterParameterGroup'
  { -- | The name of the DB cluster parameter group to modify.
    dBClusterParameterGroupName :: Types.String,
    -- | A list of parameters in the DB cluster parameter group to modify.
    parameters :: [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBClusterParameterGroup' value with any optional fields omitted.
mkModifyDBClusterParameterGroup ::
  -- | 'dBClusterParameterGroupName'
  Types.String ->
  ModifyDBClusterParameterGroup
mkModifyDBClusterParameterGroup dBClusterParameterGroupName =
  ModifyDBClusterParameterGroup'
    { dBClusterParameterGroupName,
      parameters = Core.mempty
    }

-- | The name of the DB cluster parameter group to modify.
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcpgDBClusterParameterGroupName :: Lens.Lens' ModifyDBClusterParameterGroup Types.String
mdbcpgDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED mdbcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | A list of parameters in the DB cluster parameter group to modify.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcpgParameters :: Lens.Lens' ModifyDBClusterParameterGroup [Types.Parameter]
mdbcpgParameters = Lens.field @"parameters"
{-# DEPRECATED mdbcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.AWSRequest ModifyDBClusterParameterGroup where
  type
    Rs ModifyDBClusterParameterGroup =
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
            ( Core.pure ("Action", "ModifyDBClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterParameterGroupName"
                            dBClusterParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "Parameter" parameters)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterParameterGroupResult"
      (\s h x -> Core.parseXML x)
