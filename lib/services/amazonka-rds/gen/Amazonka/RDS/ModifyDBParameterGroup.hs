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
-- Module      : Amazonka.RDS.ModifyDBParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB parameter group. To modify more than one
-- parameter, submit a list of the following: @ParameterName@,
-- @ParameterValue@, and @ApplyMethod@. A maximum of 20 parameters can be
-- modified in a single request.
--
-- After you modify a DB parameter group, you should wait at least 5
-- minutes before creating your first DB instance that uses that DB
-- parameter group as the default parameter group. This allows Amazon RDS
-- to fully complete the modify action before the parameter group is used
-- as the default for a new DB instance. This is especially important for
-- parameters that are critical when creating the default database for a DB
-- instance, such as the character set for the default database defined by
-- the @character_set_database@ parameter. You can use the /Parameter
-- Groups/ option of the
-- <https://console.aws.amazon.com/rds/ Amazon RDS console> or the
-- /DescribeDBParameters/ command to verify that your DB parameter group
-- has been created or modified.
module Amazonka.RDS.ModifyDBParameterGroup
  ( -- * Creating a Request
    ModifyDBParameterGroup (..),
    newModifyDBParameterGroup,

    -- * Request Lenses
    modifyDBParameterGroup_dbParameterGroupName,
    modifyDBParameterGroup_parameters,

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
-- /See:/ 'newModifyDBParameterGroup' smart constructor.
data ModifyDBParameterGroup = ModifyDBParameterGroup'
  { -- | The name of the DB parameter group.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing @DBParameterGroup@.
    dbParameterGroupName :: Prelude.Text,
    -- | An array of parameter names, values, and the application methods for the
    -- parameter update. At least one parameter name, value, and application
    -- method must be supplied; later arguments are optional. A maximum of 20
    -- parameters can be modified in a single request.
    --
    -- Valid Values (for the application method): @immediate | pending-reboot@
    --
    -- You can use the @immediate@ value with dynamic parameters only. You can
    -- use the @pending-reboot@ value for both dynamic and static parameters.
    --
    -- When the application method is @immediate@, changes to dynamic
    -- parameters are applied immediately to the DB instances associated with
    -- the parameter group.
    --
    -- When the application method is @pending-reboot@, changes to dynamic and
    -- static parameters are applied after a reboot without failover to the DB
    -- instances associated with the parameter group.
    --
    -- You can\'t use @pending-reboot@ with dynamic parameters on RDS for SQL
    -- Server DB instances. Use @immediate@.
    --
    -- For more information on modifying DB parameters, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithParamGroups.html Working with DB parameter groups>
    -- in the /Amazon RDS User Guide/.
    parameters :: [Parameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'modifyDBParameterGroup_dbParameterGroupName' - The name of the DB parameter group.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing @DBParameterGroup@.
--
-- 'parameters', 'modifyDBParameterGroup_parameters' - An array of parameter names, values, and the application methods for the
-- parameter update. At least one parameter name, value, and application
-- method must be supplied; later arguments are optional. A maximum of 20
-- parameters can be modified in a single request.
--
-- Valid Values (for the application method): @immediate | pending-reboot@
--
-- You can use the @immediate@ value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters.
--
-- When the application method is @immediate@, changes to dynamic
-- parameters are applied immediately to the DB instances associated with
-- the parameter group.
--
-- When the application method is @pending-reboot@, changes to dynamic and
-- static parameters are applied after a reboot without failover to the DB
-- instances associated with the parameter group.
--
-- You can\'t use @pending-reboot@ with dynamic parameters on RDS for SQL
-- Server DB instances. Use @immediate@.
--
-- For more information on modifying DB parameters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithParamGroups.html Working with DB parameter groups>
-- in the /Amazon RDS User Guide/.
newModifyDBParameterGroup ::
  -- | 'dbParameterGroupName'
  Prelude.Text ->
  ModifyDBParameterGroup
newModifyDBParameterGroup pDBParameterGroupName_ =
  ModifyDBParameterGroup'
    { dbParameterGroupName =
        pDBParameterGroupName_,
      parameters = Prelude.mempty
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing @DBParameterGroup@.
modifyDBParameterGroup_dbParameterGroupName :: Lens.Lens' ModifyDBParameterGroup Prelude.Text
modifyDBParameterGroup_dbParameterGroupName = Lens.lens (\ModifyDBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@ModifyDBParameterGroup' {} a -> s {dbParameterGroupName = a} :: ModifyDBParameterGroup)

-- | An array of parameter names, values, and the application methods for the
-- parameter update. At least one parameter name, value, and application
-- method must be supplied; later arguments are optional. A maximum of 20
-- parameters can be modified in a single request.
--
-- Valid Values (for the application method): @immediate | pending-reboot@
--
-- You can use the @immediate@ value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters.
--
-- When the application method is @immediate@, changes to dynamic
-- parameters are applied immediately to the DB instances associated with
-- the parameter group.
--
-- When the application method is @pending-reboot@, changes to dynamic and
-- static parameters are applied after a reboot without failover to the DB
-- instances associated with the parameter group.
--
-- You can\'t use @pending-reboot@ with dynamic parameters on RDS for SQL
-- Server DB instances. Use @immediate@.
--
-- For more information on modifying DB parameters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithParamGroups.html Working with DB parameter groups>
-- in the /Amazon RDS User Guide/.
modifyDBParameterGroup_parameters :: Lens.Lens' ModifyDBParameterGroup [Parameter]
modifyDBParameterGroup_parameters = Lens.lens (\ModifyDBParameterGroup' {parameters} -> parameters) (\s@ModifyDBParameterGroup' {} a -> s {parameters = a} :: ModifyDBParameterGroup) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyDBParameterGroup where
  type
    AWSResponse ModifyDBParameterGroup =
      DBParameterGroupNameMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBParameterGroupResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ModifyDBParameterGroup where
  hashWithSalt _salt ModifyDBParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData ModifyDBParameterGroup where
  rnf ModifyDBParameterGroup' {..} =
    Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf parameters

instance Data.ToHeaders ModifyDBParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBParameterGroup where
  toQuery ModifyDBParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "Parameters"
          Data.=: Data.toQueryList "Parameter" parameters
      ]
