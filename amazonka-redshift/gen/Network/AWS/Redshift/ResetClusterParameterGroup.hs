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
-- Module      : Network.AWS.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- \"engine-default\". To reset the entire parameter group specify the
-- /ResetAllParameters/ parameter. For parameter changes to take effect you
-- must reboot any associated clusters.
module Network.AWS.Redshift.ResetClusterParameterGroup
  ( -- * Creating a Request
    ResetClusterParameterGroup (..),
    newResetClusterParameterGroup,

    -- * Request Lenses
    resetClusterParameterGroup_resetAllParameters,
    resetClusterParameterGroup_parameters,
    resetClusterParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    ClusterParameterGroupNameMessage (..),
    newClusterParameterGroupNameMessage,

    -- * Response Lenses
    clusterParameterGroupNameMessage_parameterGroupStatus,
    clusterParameterGroupNameMessage_parameterGroupName,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newResetClusterParameterGroup' smart constructor.
data ResetClusterParameterGroup = ResetClusterParameterGroup'
  { -- | If @true@, all parameters in the specified parameter group will be reset
    -- to their default values.
    --
    -- Default: @true@
    resetAllParameters :: Prelude.Maybe Prelude.Bool,
    -- | An array of names of parameters to be reset. If /ResetAllParameters/
    -- option is not used, then at least one parameter name must be supplied.
    --
    -- Constraints: A maximum of 20 parameters can be reset in a single
    -- request.
    parameters :: Prelude.Maybe [Parameter],
    -- | The name of the cluster parameter group to be reset.
    parameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resetAllParameters', 'resetClusterParameterGroup_resetAllParameters' - If @true@, all parameters in the specified parameter group will be reset
-- to their default values.
--
-- Default: @true@
--
-- 'parameters', 'resetClusterParameterGroup_parameters' - An array of names of parameters to be reset. If /ResetAllParameters/
-- option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single
-- request.
--
-- 'parameterGroupName', 'resetClusterParameterGroup_parameterGroupName' - The name of the cluster parameter group to be reset.
newResetClusterParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  ResetClusterParameterGroup
newResetClusterParameterGroup pParameterGroupName_ =
  ResetClusterParameterGroup'
    { resetAllParameters =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | If @true@, all parameters in the specified parameter group will be reset
-- to their default values.
--
-- Default: @true@
resetClusterParameterGroup_resetAllParameters :: Lens.Lens' ResetClusterParameterGroup (Prelude.Maybe Prelude.Bool)
resetClusterParameterGroup_resetAllParameters = Lens.lens (\ResetClusterParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetClusterParameterGroup' {} a -> s {resetAllParameters = a} :: ResetClusterParameterGroup)

-- | An array of names of parameters to be reset. If /ResetAllParameters/
-- option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single
-- request.
resetClusterParameterGroup_parameters :: Lens.Lens' ResetClusterParameterGroup (Prelude.Maybe [Parameter])
resetClusterParameterGroup_parameters = Lens.lens (\ResetClusterParameterGroup' {parameters} -> parameters) (\s@ResetClusterParameterGroup' {} a -> s {parameters = a} :: ResetClusterParameterGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the cluster parameter group to be reset.
resetClusterParameterGroup_parameterGroupName :: Lens.Lens' ResetClusterParameterGroup Prelude.Text
resetClusterParameterGroup_parameterGroupName = Lens.lens (\ResetClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ResetClusterParameterGroup' {} a -> s {parameterGroupName = a} :: ResetClusterParameterGroup)

instance Core.AWSRequest ResetClusterParameterGroup where
  type
    AWSResponse ResetClusterParameterGroup =
      ClusterParameterGroupNameMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ResetClusterParameterGroupResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ResetClusterParameterGroup

instance Prelude.NFData ResetClusterParameterGroup

instance Core.ToHeaders ResetClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ResetClusterParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ResetClusterParameterGroup where
  toQuery ResetClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ResetClusterParameterGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ResetAllParameters" Core.=: resetAllParameters,
        "Parameters"
          Core.=: Core.toQuery
            ( Core.toQueryList "Parameter"
                Prelude.<$> parameters
            ),
        "ParameterGroupName" Core.=: parameterGroupName
      ]
