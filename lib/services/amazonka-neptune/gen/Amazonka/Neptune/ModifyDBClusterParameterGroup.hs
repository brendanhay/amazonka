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
-- Module      : Amazonka.Neptune.ModifyDBClusterParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a DB cluster parameter group. To modify more
-- than one parameter, submit a list of the following: @ParameterName@,
-- @ParameterValue@, and @ApplyMethod@. A maximum of 20 parameters can be
-- modified in a single request.
--
-- Changes to dynamic parameters are applied immediately. Changes to static
-- parameters require a reboot without failover to the DB cluster
-- associated with the parameter group before the change can take effect.
--
-- After you create a DB cluster parameter group, you should wait at least
-- 5 minutes before creating your first DB cluster that uses that DB
-- cluster parameter group as the default parameter group. This allows
-- Amazon Neptune to fully complete the create action before the parameter
-- group is used as the default for a new DB cluster. This is especially
-- important for parameters that are critical when creating the default
-- database for a DB cluster, such as the character set for the default
-- database defined by the @character_set_database@ parameter. You can use
-- the /Parameter Groups/ option of the Amazon Neptune console or the
-- DescribeDBClusterParameters command to verify that your DB cluster
-- parameter group has been created or modified.
module Amazonka.Neptune.ModifyDBClusterParameterGroup
  ( -- * Creating a Request
    ModifyDBClusterParameterGroup (..),
    newModifyDBClusterParameterGroup,

    -- * Request Lenses
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,

    -- * Destructuring the Response
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,

    -- * Response Lenses
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBClusterParameterGroup' smart constructor.
data ModifyDBClusterParameterGroup = ModifyDBClusterParameterGroup'
  { -- | The name of the DB cluster parameter group to modify.
    dbClusterParameterGroupName :: Prelude.Text,
    -- | A list of parameters in the DB cluster parameter group to modify.
    parameters :: [Parameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroupName', 'modifyDBClusterParameterGroup_dbClusterParameterGroupName' - The name of the DB cluster parameter group to modify.
--
-- 'parameters', 'modifyDBClusterParameterGroup_parameters' - A list of parameters in the DB cluster parameter group to modify.
newModifyDBClusterParameterGroup ::
  -- | 'dbClusterParameterGroupName'
  Prelude.Text ->
  ModifyDBClusterParameterGroup
newModifyDBClusterParameterGroup
  pDBClusterParameterGroupName_ =
    ModifyDBClusterParameterGroup'
      { dbClusterParameterGroupName =
          pDBClusterParameterGroupName_,
        parameters = Prelude.mempty
      }

-- | The name of the DB cluster parameter group to modify.
modifyDBClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' ModifyDBClusterParameterGroup Prelude.Text
modifyDBClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\ModifyDBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@ModifyDBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: ModifyDBClusterParameterGroup)

-- | A list of parameters in the DB cluster parameter group to modify.
modifyDBClusterParameterGroup_parameters :: Lens.Lens' ModifyDBClusterParameterGroup [Parameter]
modifyDBClusterParameterGroup_parameters = Lens.lens (\ModifyDBClusterParameterGroup' {parameters} -> parameters) (\s@ModifyDBClusterParameterGroup' {} a -> s {parameters = a} :: ModifyDBClusterParameterGroup) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    ModifyDBClusterParameterGroup
  where
  type
    AWSResponse ModifyDBClusterParameterGroup =
      DBClusterParameterGroupNameMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterParameterGroupResult"
      (\s h x -> Core.parseXML x)

instance
  Prelude.Hashable
    ModifyDBClusterParameterGroup
  where
  hashWithSalt _salt ModifyDBClusterParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData ModifyDBClusterParameterGroup where
  rnf ModifyDBClusterParameterGroup' {..} =
    Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf parameters

instance Core.ToHeaders ModifyDBClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBClusterParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBClusterParameterGroup where
  toQuery ModifyDBClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyDBClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "Parameters"
          Core.=: Core.toQueryList "Parameter" parameters
      ]
