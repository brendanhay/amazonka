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
-- Module      : Amazonka.CloudFormation.ImportStacksToStackSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import existing stacks into a new stack sets. Use the stack import
-- operation to import up to 10 stacks into a new stack set in the same
-- account as the source stack or in a different administrator account and
-- Region, by specifying the stack ID of the stack you intend to import.
module Amazonka.CloudFormation.ImportStacksToStackSet
  ( -- * Creating a Request
    ImportStacksToStackSet (..),
    newImportStacksToStackSet,

    -- * Request Lenses
    importStacksToStackSet_callAs,
    importStacksToStackSet_operationId,
    importStacksToStackSet_operationPreferences,
    importStacksToStackSet_organizationalUnitIds,
    importStacksToStackSet_stackIds,
    importStacksToStackSet_stackIdsUrl,
    importStacksToStackSet_stackSetName,

    -- * Destructuring the Response
    ImportStacksToStackSetResponse (..),
    newImportStacksToStackSetResponse,

    -- * Response Lenses
    importStacksToStackSetResponse_operationId,
    importStacksToStackSetResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportStacksToStackSet' smart constructor.
data ImportStacksToStackSet = ImportStacksToStackSet'
  { -- | By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   For service managed stack sets, specify @DELEGATED_ADMIN@.
    callAs :: Prelude.Maybe CallAs,
    -- | A unique, user defined, identifier for the stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The user-specified preferences for how CloudFormation performs a stack
    -- set operation.
    --
    -- For more information about maximum concurrent accounts and failure
    -- tolerance, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | The list of OU ID\'s to which the stacks being imported has to be mapped
    -- as deployment target.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the stacks you are importing into a stack set. You import up
    -- to 10 stacks per stack set at a time.
    --
    -- Specify either @StackIds@ or @StackIdsUrl@.
    stackIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon S3 URL which contains list of stack ids to be inputted.
    --
    -- Specify either @StackIds@ or @StackIdsUrl@.
    stackIdsUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the stack set. The name must be unique in the Region where
    -- you create your stack set.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportStacksToStackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAs', 'importStacksToStackSet_callAs' - By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   For service managed stack sets, specify @DELEGATED_ADMIN@.
--
-- 'operationId', 'importStacksToStackSet_operationId' - A unique, user defined, identifier for the stack set operation.
--
-- 'operationPreferences', 'importStacksToStackSet_operationPreferences' - The user-specified preferences for how CloudFormation performs a stack
-- set operation.
--
-- For more information about maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
--
-- 'organizationalUnitIds', 'importStacksToStackSet_organizationalUnitIds' - The list of OU ID\'s to which the stacks being imported has to be mapped
-- as deployment target.
--
-- 'stackIds', 'importStacksToStackSet_stackIds' - The IDs of the stacks you are importing into a stack set. You import up
-- to 10 stacks per stack set at a time.
--
-- Specify either @StackIds@ or @StackIdsUrl@.
--
-- 'stackIdsUrl', 'importStacksToStackSet_stackIdsUrl' - The Amazon S3 URL which contains list of stack ids to be inputted.
--
-- Specify either @StackIds@ or @StackIdsUrl@.
--
-- 'stackSetName', 'importStacksToStackSet_stackSetName' - The name of the stack set. The name must be unique in the Region where
-- you create your stack set.
newImportStacksToStackSet ::
  -- | 'stackSetName'
  Prelude.Text ->
  ImportStacksToStackSet
newImportStacksToStackSet pStackSetName_ =
  ImportStacksToStackSet'
    { callAs = Prelude.Nothing,
      operationId = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      organizationalUnitIds = Prelude.Nothing,
      stackIds = Prelude.Nothing,
      stackIdsUrl = Prelude.Nothing,
      stackSetName = pStackSetName_
    }

-- | By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   For service managed stack sets, specify @DELEGATED_ADMIN@.
importStacksToStackSet_callAs :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe CallAs)
importStacksToStackSet_callAs = Lens.lens (\ImportStacksToStackSet' {callAs} -> callAs) (\s@ImportStacksToStackSet' {} a -> s {callAs = a} :: ImportStacksToStackSet)

-- | A unique, user defined, identifier for the stack set operation.
importStacksToStackSet_operationId :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe Prelude.Text)
importStacksToStackSet_operationId = Lens.lens (\ImportStacksToStackSet' {operationId} -> operationId) (\s@ImportStacksToStackSet' {} a -> s {operationId = a} :: ImportStacksToStackSet)

-- | The user-specified preferences for how CloudFormation performs a stack
-- set operation.
--
-- For more information about maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
importStacksToStackSet_operationPreferences :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe StackSetOperationPreferences)
importStacksToStackSet_operationPreferences = Lens.lens (\ImportStacksToStackSet' {operationPreferences} -> operationPreferences) (\s@ImportStacksToStackSet' {} a -> s {operationPreferences = a} :: ImportStacksToStackSet)

-- | The list of OU ID\'s to which the stacks being imported has to be mapped
-- as deployment target.
importStacksToStackSet_organizationalUnitIds :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe [Prelude.Text])
importStacksToStackSet_organizationalUnitIds = Lens.lens (\ImportStacksToStackSet' {organizationalUnitIds} -> organizationalUnitIds) (\s@ImportStacksToStackSet' {} a -> s {organizationalUnitIds = a} :: ImportStacksToStackSet) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the stacks you are importing into a stack set. You import up
-- to 10 stacks per stack set at a time.
--
-- Specify either @StackIds@ or @StackIdsUrl@.
importStacksToStackSet_stackIds :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe [Prelude.Text])
importStacksToStackSet_stackIds = Lens.lens (\ImportStacksToStackSet' {stackIds} -> stackIds) (\s@ImportStacksToStackSet' {} a -> s {stackIds = a} :: ImportStacksToStackSet) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 URL which contains list of stack ids to be inputted.
--
-- Specify either @StackIds@ or @StackIdsUrl@.
importStacksToStackSet_stackIdsUrl :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe Prelude.Text)
importStacksToStackSet_stackIdsUrl = Lens.lens (\ImportStacksToStackSet' {stackIdsUrl} -> stackIdsUrl) (\s@ImportStacksToStackSet' {} a -> s {stackIdsUrl = a} :: ImportStacksToStackSet)

-- | The name of the stack set. The name must be unique in the Region where
-- you create your stack set.
importStacksToStackSet_stackSetName :: Lens.Lens' ImportStacksToStackSet Prelude.Text
importStacksToStackSet_stackSetName = Lens.lens (\ImportStacksToStackSet' {stackSetName} -> stackSetName) (\s@ImportStacksToStackSet' {} a -> s {stackSetName = a} :: ImportStacksToStackSet)

instance Core.AWSRequest ImportStacksToStackSet where
  type
    AWSResponse ImportStacksToStackSet =
      ImportStacksToStackSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ImportStacksToStackSetResult"
      ( \s h x ->
          ImportStacksToStackSetResponse'
            Prelude.<$> (x Data..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportStacksToStackSet where
  hashWithSalt _salt ImportStacksToStackSet' {..} =
    _salt
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` operationPreferences
      `Prelude.hashWithSalt` organizationalUnitIds
      `Prelude.hashWithSalt` stackIds
      `Prelude.hashWithSalt` stackIdsUrl
      `Prelude.hashWithSalt` stackSetName

instance Prelude.NFData ImportStacksToStackSet where
  rnf ImportStacksToStackSet' {..} =
    Prelude.rnf callAs
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf operationPreferences
      `Prelude.seq` Prelude.rnf organizationalUnitIds
      `Prelude.seq` Prelude.rnf stackIds
      `Prelude.seq` Prelude.rnf stackIdsUrl
      `Prelude.seq` Prelude.rnf stackSetName

instance Data.ToHeaders ImportStacksToStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ImportStacksToStackSet where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportStacksToStackSet where
  toQuery ImportStacksToStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ImportStacksToStackSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "CallAs" Data.=: callAs,
        "OperationId" Data.=: operationId,
        "OperationPreferences" Data.=: operationPreferences,
        "OrganizationalUnitIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> organizationalUnitIds
            ),
        "StackIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> stackIds),
        "StackIdsUrl" Data.=: stackIdsUrl,
        "StackSetName" Data.=: stackSetName
      ]

-- | /See:/ 'newImportStacksToStackSetResponse' smart constructor.
data ImportStacksToStackSetResponse = ImportStacksToStackSetResponse'
  { -- | The unique identifier for the stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportStacksToStackSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'importStacksToStackSetResponse_operationId' - The unique identifier for the stack set operation.
--
-- 'httpStatus', 'importStacksToStackSetResponse_httpStatus' - The response's http status code.
newImportStacksToStackSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportStacksToStackSetResponse
newImportStacksToStackSetResponse pHttpStatus_ =
  ImportStacksToStackSetResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the stack set operation.
importStacksToStackSetResponse_operationId :: Lens.Lens' ImportStacksToStackSetResponse (Prelude.Maybe Prelude.Text)
importStacksToStackSetResponse_operationId = Lens.lens (\ImportStacksToStackSetResponse' {operationId} -> operationId) (\s@ImportStacksToStackSetResponse' {} a -> s {operationId = a} :: ImportStacksToStackSetResponse)

-- | The response's http status code.
importStacksToStackSetResponse_httpStatus :: Lens.Lens' ImportStacksToStackSetResponse Prelude.Int
importStacksToStackSetResponse_httpStatus = Lens.lens (\ImportStacksToStackSetResponse' {httpStatus} -> httpStatus) (\s@ImportStacksToStackSetResponse' {} a -> s {httpStatus = a} :: ImportStacksToStackSetResponse)

instance
  Prelude.NFData
    ImportStacksToStackSetResponse
  where
  rnf ImportStacksToStackSetResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
