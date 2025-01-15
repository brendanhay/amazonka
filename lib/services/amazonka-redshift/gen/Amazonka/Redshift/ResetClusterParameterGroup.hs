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
-- Module      : Amazonka.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- \"engine-default\". To reset the entire parameter group specify the
-- /ResetAllParameters/ parameter. For parameter changes to take effect you
-- must reboot any associated clusters.
module Amazonka.Redshift.ResetClusterParameterGroup
  ( -- * Creating a Request
    ResetClusterParameterGroup (..),
    newResetClusterParameterGroup,

    -- * Request Lenses
    resetClusterParameterGroup_parameters,
    resetClusterParameterGroup_resetAllParameters,
    resetClusterParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    ClusterParameterGroupNameMessage (..),
    newClusterParameterGroupNameMessage,

    -- * Response Lenses
    clusterParameterGroupNameMessage_parameterGroupName,
    clusterParameterGroupNameMessage_parameterGroupStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newResetClusterParameterGroup' smart constructor.
data ResetClusterParameterGroup = ResetClusterParameterGroup'
  { -- | An array of names of parameters to be reset. If /ResetAllParameters/
    -- option is not used, then at least one parameter name must be supplied.
    --
    -- Constraints: A maximum of 20 parameters can be reset in a single
    -- request.
    parameters :: Prelude.Maybe [Parameter],
    -- | If @true@, all parameters in the specified parameter group will be reset
    -- to their default values.
    --
    -- Default: @true@
    resetAllParameters :: Prelude.Maybe Prelude.Bool,
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
-- 'parameters', 'resetClusterParameterGroup_parameters' - An array of names of parameters to be reset. If /ResetAllParameters/
-- option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single
-- request.
--
-- 'resetAllParameters', 'resetClusterParameterGroup_resetAllParameters' - If @true@, all parameters in the specified parameter group will be reset
-- to their default values.
--
-- Default: @true@
--
-- 'parameterGroupName', 'resetClusterParameterGroup_parameterGroupName' - The name of the cluster parameter group to be reset.
newResetClusterParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  ResetClusterParameterGroup
newResetClusterParameterGroup pParameterGroupName_ =
  ResetClusterParameterGroup'
    { parameters =
        Prelude.Nothing,
      resetAllParameters = Prelude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An array of names of parameters to be reset. If /ResetAllParameters/
-- option is not used, then at least one parameter name must be supplied.
--
-- Constraints: A maximum of 20 parameters can be reset in a single
-- request.
resetClusterParameterGroup_parameters :: Lens.Lens' ResetClusterParameterGroup (Prelude.Maybe [Parameter])
resetClusterParameterGroup_parameters = Lens.lens (\ResetClusterParameterGroup' {parameters} -> parameters) (\s@ResetClusterParameterGroup' {} a -> s {parameters = a} :: ResetClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | If @true@, all parameters in the specified parameter group will be reset
-- to their default values.
--
-- Default: @true@
resetClusterParameterGroup_resetAllParameters :: Lens.Lens' ResetClusterParameterGroup (Prelude.Maybe Prelude.Bool)
resetClusterParameterGroup_resetAllParameters = Lens.lens (\ResetClusterParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetClusterParameterGroup' {} a -> s {resetAllParameters = a} :: ResetClusterParameterGroup)

-- | The name of the cluster parameter group to be reset.
resetClusterParameterGroup_parameterGroupName :: Lens.Lens' ResetClusterParameterGroup Prelude.Text
resetClusterParameterGroup_parameterGroupName = Lens.lens (\ResetClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ResetClusterParameterGroup' {} a -> s {parameterGroupName = a} :: ResetClusterParameterGroup)

instance Core.AWSRequest ResetClusterParameterGroup where
  type
    AWSResponse ResetClusterParameterGroup =
      ClusterParameterGroupNameMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ResetClusterParameterGroupResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ResetClusterParameterGroup where
  hashWithSalt _salt ResetClusterParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resetAllParameters
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData ResetClusterParameterGroup where
  rnf ResetClusterParameterGroup' {..} =
    Prelude.rnf parameters `Prelude.seq`
      Prelude.rnf resetAllParameters `Prelude.seq`
        Prelude.rnf parameterGroupName

instance Data.ToHeaders ResetClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetClusterParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetClusterParameterGroup where
  toQuery ResetClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetClusterParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Parameters"
          Data.=: Data.toQuery
            ( Data.toQueryList "Parameter"
                Prelude.<$> parameters
            ),
        "ResetAllParameters" Data.=: resetAllParameters,
        "ParameterGroupName" Data.=: parameterGroupName
      ]
