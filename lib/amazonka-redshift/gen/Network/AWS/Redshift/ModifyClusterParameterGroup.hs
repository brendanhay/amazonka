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
    mcpgParameters,
    mcpgParameterGroupName,

    -- * Destructuring the response
    ClusterParameterGroupNameMessage (..),
    mkClusterParameterGroupNameMessage,

    -- ** Response lenses
    cpgnmParameterGroupStatus,
    cpgnmParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Describes a modify cluster parameter group operation.
--
-- /See:/ 'mkModifyClusterParameterGroup' smart constructor.
data ModifyClusterParameterGroup = ModifyClusterParameterGroup'
  { -- | An array of parameters to be modified. A maximum of 20 parameters can be modified in a single request.
    --
    -- For each parameter to be modified, you must supply at least the parameter name and parameter value; other name-value pairs of the parameter are optional.
    -- For the workload management (WLM) configuration, you must supply all the name-value pairs in the wlm_json_configuration parameter.
    parameters :: [Parameter],
    -- | The name of the parameter group to be modified.
    parameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'parameters' - An array of parameters to be modified. A maximum of 20 parameters can be modified in a single request.
--
-- For each parameter to be modified, you must supply at least the parameter name and parameter value; other name-value pairs of the parameter are optional.
-- For the workload management (WLM) configuration, you must supply all the name-value pairs in the wlm_json_configuration parameter.
-- * 'parameterGroupName' - The name of the parameter group to be modified.
mkModifyClusterParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  ModifyClusterParameterGroup
mkModifyClusterParameterGroup pParameterGroupName_ =
  ModifyClusterParameterGroup'
    { parameters = Lude.mempty,
      parameterGroupName = pParameterGroupName_
    }

-- | An array of parameters to be modified. A maximum of 20 parameters can be modified in a single request.
--
-- For each parameter to be modified, you must supply at least the parameter name and parameter value; other name-value pairs of the parameter are optional.
-- For the workload management (WLM) configuration, you must supply all the name-value pairs in the wlm_json_configuration parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameters :: Lens.Lens' ModifyClusterParameterGroup [Parameter]
mcpgParameters = Lens.lens (parameters :: ModifyClusterParameterGroup -> [Parameter]) (\s a -> s {parameters = a} :: ModifyClusterParameterGroup)
{-# DEPRECATED mcpgParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the parameter group to be modified.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpgParameterGroupName :: Lens.Lens' ModifyClusterParameterGroup Lude.Text
mcpgParameterGroupName = Lens.lens (parameterGroupName :: ModifyClusterParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: ModifyClusterParameterGroup)
{-# DEPRECATED mcpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.AWSRequest ModifyClusterParameterGroup where
  type
    Rs ModifyClusterParameterGroup =
      ClusterParameterGroupNameMessage
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterParameterGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyClusterParameterGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterParameterGroup where
  toQuery ModifyClusterParameterGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyClusterParameterGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Parameters" Lude.=: Lude.toQueryList "Parameter" parameters,
        "ParameterGroupName" Lude.=: parameterGroupName
      ]
