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
-- Module      : Amazonka.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB parameter group to the engine\/system
-- default value. To reset specific parameters, provide a list of the
-- following: @ParameterName@ and @ApplyMethod@. To reset the entire DB
-- parameter group, specify the @DBParameterGroup@ name and
-- @ResetAllParameters@ parameters. When resetting the entire group,
-- dynamic parameters are updated immediately and static parameters are set
-- to @pending-reboot@ to take effect on the next DB instance restart or
-- @RebootDBInstance@ request.
module Amazonka.RDS.ResetDBParameterGroup
  ( -- * Creating a Request
    ResetDBParameterGroup (..),
    newResetDBParameterGroup,

    -- * Request Lenses
    resetDBParameterGroup_parameters,
    resetDBParameterGroup_resetAllParameters,
    resetDBParameterGroup_dbParameterGroupName,

    -- * Destructuring the Response
    DBParameterGroupNameMessage (..),
    newDBParameterGroupNameMessage,

    -- * Response Lenses
    dbParameterGroupNameMessage_dbParameterGroupName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newResetDBParameterGroup' smart constructor.
data ResetDBParameterGroup = ResetDBParameterGroup'
  { -- | To reset the entire DB parameter group, specify the @DBParameterGroup@
    -- name and @ResetAllParameters@ parameters. To reset specific parameters,
    -- provide a list of the following: @ParameterName@ and @ApplyMethod@. A
    -- maximum of 20 parameters can be modified in a single request.
    --
    -- __MySQL__
    --
    -- Valid Values (for Apply method): @immediate@ | @pending-reboot@
    --
    -- You can use the immediate value with dynamic parameters only. You can
    -- use the @pending-reboot@ value for both dynamic and static parameters,
    -- and changes are applied when DB instance reboots.
    --
    -- __MariaDB__
    --
    -- Valid Values (for Apply method): @immediate@ | @pending-reboot@
    --
    -- You can use the immediate value with dynamic parameters only. You can
    -- use the @pending-reboot@ value for both dynamic and static parameters,
    -- and changes are applied when DB instance reboots.
    --
    -- __Oracle__
    --
    -- Valid Values (for Apply method): @pending-reboot@
    parameters :: Prelude.Maybe [Parameter],
    -- | A value that indicates whether to reset all parameters in the DB
    -- parameter group to default values. By default, all parameters in the DB
    -- parameter group are reset to default values.
    resetAllParameters :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    -- -   Must match the name of an existing @DBParameterGroup@.
    dbParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetDBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'resetDBParameterGroup_parameters' - To reset the entire DB parameter group, specify the @DBParameterGroup@
-- name and @ResetAllParameters@ parameters. To reset specific parameters,
-- provide a list of the following: @ParameterName@ and @ApplyMethod@. A
-- maximum of 20 parameters can be modified in a single request.
--
-- __MySQL__
--
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
--
-- You can use the immediate value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters,
-- and changes are applied when DB instance reboots.
--
-- __MariaDB__
--
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
--
-- You can use the immediate value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters,
-- and changes are applied when DB instance reboots.
--
-- __Oracle__
--
-- Valid Values (for Apply method): @pending-reboot@
--
-- 'resetAllParameters', 'resetDBParameterGroup_resetAllParameters' - A value that indicates whether to reset all parameters in the DB
-- parameter group to default values. By default, all parameters in the DB
-- parameter group are reset to default values.
--
-- 'dbParameterGroupName', 'resetDBParameterGroup_dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must match the name of an existing @DBParameterGroup@.
newResetDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Prelude.Text ->
  ResetDBParameterGroup
newResetDBParameterGroup pDBParameterGroupName_ =
  ResetDBParameterGroup'
    { parameters =
        Prelude.Nothing,
      resetAllParameters = Prelude.Nothing,
      dbParameterGroupName = pDBParameterGroupName_
    }

-- | To reset the entire DB parameter group, specify the @DBParameterGroup@
-- name and @ResetAllParameters@ parameters. To reset specific parameters,
-- provide a list of the following: @ParameterName@ and @ApplyMethod@. A
-- maximum of 20 parameters can be modified in a single request.
--
-- __MySQL__
--
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
--
-- You can use the immediate value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters,
-- and changes are applied when DB instance reboots.
--
-- __MariaDB__
--
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
--
-- You can use the immediate value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters,
-- and changes are applied when DB instance reboots.
--
-- __Oracle__
--
-- Valid Values (for Apply method): @pending-reboot@
resetDBParameterGroup_parameters :: Lens.Lens' ResetDBParameterGroup (Prelude.Maybe [Parameter])
resetDBParameterGroup_parameters = Lens.lens (\ResetDBParameterGroup' {parameters} -> parameters) (\s@ResetDBParameterGroup' {} a -> s {parameters = a} :: ResetDBParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to reset all parameters in the DB
-- parameter group to default values. By default, all parameters in the DB
-- parameter group are reset to default values.
resetDBParameterGroup_resetAllParameters :: Lens.Lens' ResetDBParameterGroup (Prelude.Maybe Prelude.Bool)
resetDBParameterGroup_resetAllParameters = Lens.lens (\ResetDBParameterGroup' {resetAllParameters} -> resetAllParameters) (\s@ResetDBParameterGroup' {} a -> s {resetAllParameters = a} :: ResetDBParameterGroup)

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must match the name of an existing @DBParameterGroup@.
resetDBParameterGroup_dbParameterGroupName :: Lens.Lens' ResetDBParameterGroup Prelude.Text
resetDBParameterGroup_dbParameterGroupName = Lens.lens (\ResetDBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@ResetDBParameterGroup' {} a -> s {dbParameterGroupName = a} :: ResetDBParameterGroup)

instance Core.AWSRequest ResetDBParameterGroup where
  type
    AWSResponse ResetDBParameterGroup =
      DBParameterGroupNameMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ResetDBParameterGroupResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ResetDBParameterGroup where
  hashWithSalt _salt ResetDBParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resetAllParameters
      `Prelude.hashWithSalt` dbParameterGroupName

instance Prelude.NFData ResetDBParameterGroup where
  rnf ResetDBParameterGroup' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resetAllParameters
      `Prelude.seq` Prelude.rnf dbParameterGroupName

instance Data.ToHeaders ResetDBParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetDBParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetDBParameterGroup where
  toQuery ResetDBParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetDBParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Parameters"
          Data.=: Data.toQuery
            ( Data.toQueryList "Parameter"
                Prelude.<$> parameters
            ),
        "ResetAllParameters" Data.=: resetAllParameters,
        "DBParameterGroupName" Data.=: dbParameterGroupName
      ]
