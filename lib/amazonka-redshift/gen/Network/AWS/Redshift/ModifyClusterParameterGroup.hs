{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.ModifyClusterParameterGroup
  ( -- * Creating a request
    ModifyClusterParameterGroup (..),
    mkModifyClusterParameterGroup,

    -- ** Request lenses
    mcpgParameterGroupName,
    mcpgParameters,

    -- * Destructuring the response
    Types.ClusterParameterGroupNameMessage (..),
    Types.mkClusterParameterGroupNameMessage,

    -- ** Response lenses
    Types.cpgnmParameterGroupName,
    Types.cpgnmParameterGroupStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a modify cluster parameter group operation.
--
-- /See:/ 'mkModifyClusterParameterGroup' smart constructor.
data ModifyClusterParameterGroup = ModifyClusterParameterGroup'
  { -- | The name of the parameter group to be modified.
    parameterGroupName :: Types.ParameterGroupName,
    -- | An array of parameters to be modified. A maximum of 20 parameters can be modified in a single request.
    --
    -- For each parameter to be modified, you must supply at least the parameter name and parameter value; other name-value pairs of the parameter are optional.
    -- For the workload management (WLM) configuration, you must supply all the name-value pairs in the wlm_json_configuration parameter.
    parameters :: [Types.Parameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyClusterParameterGroup' value with any optional fields omitted.
mkModifyClusterParameterGroup ::
  -- | 'parameterGroupName'
  Types.ParameterGroupName ->
  ModifyClusterParameterGroup
mkModifyClusterParameterGroup parameterGroupName =
  ModifyClusterParameterGroup'
    { parameterGroupName,
      parameters = Core.mempty
    }

-- | The name of the parameter group to be modified.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameterGroupName :: Lens.Lens' ModifyClusterParameterGroup Types.ParameterGroupName
mcpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED mcpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | An array of parameters to be modified. A maximum of 20 parameters can be modified in a single request.
--
-- For each parameter to be modified, you must supply at least the parameter name and parameter value; other name-value pairs of the parameter are optional.
-- For the workload management (WLM) configuration, you must supply all the name-value pairs in the wlm_json_configuration parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameters :: Lens.Lens' ModifyClusterParameterGroup [Types.Parameter]
mcpgParameters = Lens.field @"parameters"
{-# DEPRECATED mcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.AWSRequest ModifyClusterParameterGroup where
  type
    Rs ModifyClusterParameterGroup =
      Types.ClusterParameterGroupNameMessage
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
            ( Core.pure ("Action", "ModifyClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ParameterGroupName" parameterGroupName)
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "Parameter" parameters)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyClusterParameterGroupResult"
      (\s h x -> Core.parseXML x)
