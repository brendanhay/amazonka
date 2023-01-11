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
-- Module      : Amazonka.Neptune.ResetDBClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group to the default
-- value. To reset specific parameters submit a list of the following:
-- @ParameterName@ and @ApplyMethod@. To reset the entire DB cluster
-- parameter group, specify the @DBClusterParameterGroupName@ and
-- @ResetAllParameters@ parameters.
--
-- When resetting the entire group, dynamic parameters are updated
-- immediately and static parameters are set to @pending-reboot@ to take
-- effect on the next DB instance restart or RebootDBInstance request. You
-- must call RebootDBInstance for every DB instance in your DB cluster that
-- you want the updated static parameter to apply to.
module Amazonka.Neptune.ResetDBClusterParameterGroup
  ( -- * Creating a Request
    ResetDBClusterParameterGroup (..),
    newResetDBClusterParameterGroup,

    -- * Request Lenses
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,

    -- * Destructuring the Response
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,

    -- * Response Lenses
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetDBClusterParameterGroup' smart constructor.
data ResetDBClusterParameterGroup = ResetDBClusterParameterGroup'
  { -- | A list of parameter names in the DB cluster parameter group to reset to
    -- the default values. You can\'t use this parameter if the
    -- @ResetAllParameters@ parameter is set to @true@.
    parameters :: Prelude.Maybe [Parameter],
    -- | A value that is set to @true@ to reset all parameters in the DB cluster
    -- parameter group to their default values, and @false@ otherwise. You
    -- can\'t use this parameter if there is a list of parameter names
    -- specified for the @Parameters@ parameter.
    resetAllParameters :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB cluster parameter group to reset.
    dbClusterParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetDBClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'resetDBClusterParameterGroup_parameters' - A list of parameter names in the DB cluster parameter group to reset to
-- the default values. You can\'t use this parameter if the
-- @ResetAllParameters@ parameter is set to @true@.
--
-- 'resetAllParameters', 'resetDBClusterParameterGroup_resetAllParameters' - A value that is set to @true@ to reset all parameters in the DB cluster
-- parameter group to their default values, and @false@ otherwise. You
-- can\'t use this parameter if there is a list of parameter names
-- specified for the @Parameters@ parameter.
--
-- 'dbClusterParameterGroupName', 'resetDBClusterParameterGroup_dbClusterParameterGroupName' - The name of the DB cluster parameter group to reset.
newResetDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Prelude.Text ->
  ResetDBClusterParameterGroup
newResetDBClusterParameterGroup
  pDBClusterParameterGroupName_ =
    ResetDBClusterParameterGroup'
      { parameters =
          Prelude.Nothing,
        resetAllParameters = Prelude.Nothing,
        dbClusterParameterGroupName =
          pDBClusterParameterGroupName_
      }

-- | A list of parameter names in the DB cluster parameter group to reset to
-- the default values. You can\'t use this parameter if the
-- @ResetAllParameters@ parameter is set to @true@.
resetDBClusterParameterGroup_parameters :: Lens.Lens' ResetDBClusterParameterGroup (Prelude.Maybe [Parameter])
resetDBClusterParameterGroup_parameters = Lens.lens (\ResetDBClusterParameterGroup' {parameters} -> parameters) (\s@ResetDBClusterParameterGroup' {} a -> s {parameters = a} :: ResetDBClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | A value that is set to @true@ to reset all parameters in the DB cluster
-- parameter group to their default values, and @false@ otherwise. You
-- can\'t use this parameter if there is a list of parameter names
-- specified for the @Parameters@ parameter.
resetDBClusterParameterGroup_resetAllParameters :: Lens.Lens' ResetDBClusterParameterGroup (Prelude.Maybe Prelude.Bool)
resetDBClusterParameterGroup_resetAllParameters = Lens.lens (\ResetDBClusterParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetDBClusterParameterGroup' {} a -> s {resetAllParameters = a} :: ResetDBClusterParameterGroup)

-- | The name of the DB cluster parameter group to reset.
resetDBClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' ResetDBClusterParameterGroup Prelude.Text
resetDBClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\ResetDBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@ResetDBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: ResetDBClusterParameterGroup)

instance Core.AWSRequest ResetDBClusterParameterGroup where
  type
    AWSResponse ResetDBClusterParameterGroup =
      DBClusterParameterGroupNameMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ResetDBClusterParameterGroupResult"
      (\s h x -> Data.parseXML x)

instance
  Prelude.Hashable
    ResetDBClusterParameterGroup
  where
  hashWithSalt _salt ResetDBClusterParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resetAllParameters
      `Prelude.hashWithSalt` dbClusterParameterGroupName

instance Prelude.NFData ResetDBClusterParameterGroup where
  rnf ResetDBClusterParameterGroup' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resetAllParameters
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName

instance Data.ToHeaders ResetDBClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetDBClusterParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetDBClusterParameterGroup where
  toQuery ResetDBClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ResetDBClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Parameters"
          Data.=: Data.toQuery
            ( Data.toQueryList "Parameter"
                Prelude.<$> parameters
            ),
        "ResetAllParameters" Data.=: resetAllParameters,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName
      ]
