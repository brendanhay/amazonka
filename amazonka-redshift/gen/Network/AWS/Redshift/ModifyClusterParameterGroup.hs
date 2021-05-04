{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.ModifyClusterParameterGroup
  ( -- * Creating a Request
    ModifyClusterParameterGroup (..),
    newModifyClusterParameterGroup,

    -- * Request Lenses
    modifyClusterParameterGroup_parameterGroupName,
    modifyClusterParameterGroup_parameters,

    -- * Destructuring the Response
    ClusterParameterGroupNameMessage (..),
    newClusterParameterGroupNameMessage,

    -- * Response Lenses
    clusterParameterGroupNameMessage_parameterGroupStatus,
    clusterParameterGroupNameMessage_parameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a modify cluster parameter group operation.
--
-- /See:/ 'newModifyClusterParameterGroup' smart constructor.
data ModifyClusterParameterGroup = ModifyClusterParameterGroup'
  { -- | The name of the parameter group to be modified.
    parameterGroupName :: Prelude.Text,
    -- | An array of parameters to be modified. A maximum of 20 parameters can be
    -- modified in a single request.
    --
    -- For each parameter to be modified, you must supply at least the
    -- parameter name and parameter value; other name-value pairs of the
    -- parameter are optional.
    --
    -- For the workload management (WLM) configuration, you must supply all the
    -- name-value pairs in the wlm_json_configuration parameter.
    parameters :: [Parameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'modifyClusterParameterGroup_parameterGroupName' - The name of the parameter group to be modified.
--
-- 'parameters', 'modifyClusterParameterGroup_parameters' - An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request.
--
-- For each parameter to be modified, you must supply at least the
-- parameter name and parameter value; other name-value pairs of the
-- parameter are optional.
--
-- For the workload management (WLM) configuration, you must supply all the
-- name-value pairs in the wlm_json_configuration parameter.
newModifyClusterParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  ModifyClusterParameterGroup
newModifyClusterParameterGroup pParameterGroupName_ =
  ModifyClusterParameterGroup'
    { parameterGroupName =
        pParameterGroupName_,
      parameters = Prelude.mempty
    }

-- | The name of the parameter group to be modified.
modifyClusterParameterGroup_parameterGroupName :: Lens.Lens' ModifyClusterParameterGroup Prelude.Text
modifyClusterParameterGroup_parameterGroupName = Lens.lens (\ModifyClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ModifyClusterParameterGroup' {} a -> s {parameterGroupName = a} :: ModifyClusterParameterGroup)

-- | An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request.
--
-- For each parameter to be modified, you must supply at least the
-- parameter name and parameter value; other name-value pairs of the
-- parameter are optional.
--
-- For the workload management (WLM) configuration, you must supply all the
-- name-value pairs in the wlm_json_configuration parameter.
modifyClusterParameterGroup_parameters :: Lens.Lens' ModifyClusterParameterGroup [Parameter]
modifyClusterParameterGroup_parameters = Lens.lens (\ModifyClusterParameterGroup' {parameters} -> parameters) (\s@ModifyClusterParameterGroup' {} a -> s {parameters = a} :: ModifyClusterParameterGroup) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    ModifyClusterParameterGroup
  where
  type
    Rs ModifyClusterParameterGroup =
      ClusterParameterGroupNameMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyClusterParameterGroupResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable ModifyClusterParameterGroup

instance Prelude.NFData ModifyClusterParameterGroup

instance
  Prelude.ToHeaders
    ModifyClusterParameterGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyClusterParameterGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyClusterParameterGroup where
  toQuery ModifyClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyClusterParameterGroup" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ParameterGroupName" Prelude.=: parameterGroupName,
        "Parameters"
          Prelude.=: Prelude.toQueryList "Parameter" parameters
      ]
