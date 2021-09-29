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
-- Module      : Network.AWS.CloudFormation.ImportStacksToStackSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import existing stacks into a new stack sets. Use the stack import
-- operation to import up to 10 stacks into a new stack set in the same
-- account as the source stack or in a different administrator account and
-- Region, by specifying the stack ID of the stack you intend to import.
--
-- @ImportStacksToStackSet@ is only supported by self-managed permissions.
module Network.AWS.CloudFormation.ImportStacksToStackSet
  ( -- * Creating a Request
    ImportStacksToStackSet (..),
    newImportStacksToStackSet,

    -- * Request Lenses
    importStacksToStackSet_operationId,
    importStacksToStackSet_callAs,
    importStacksToStackSet_operationPreferences,
    importStacksToStackSet_stackSetName,
    importStacksToStackSet_stackIds,

    -- * Destructuring the Response
    ImportStacksToStackSetResponse (..),
    newImportStacksToStackSetResponse,

    -- * Response Lenses
    importStacksToStackSetResponse_operationId,
    importStacksToStackSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportStacksToStackSet' smart constructor.
data ImportStacksToStackSet = ImportStacksToStackSet'
  { -- | A unique, user defined, identifier for the stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   For service managed stack sets, specify @DELEGATED_ADMIN@.
    callAs :: Prelude.Maybe CallAs,
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | The name of the stack set. The name must be unique in the Region where
    -- you create your stack set.
    stackSetName :: Prelude.Text,
    -- | The IDs of the stacks you are importing into a stack set. You import up
    -- to 10 stacks per stack set at a time.
    stackIds :: [Prelude.Text]
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
-- 'operationId', 'importStacksToStackSet_operationId' - A unique, user defined, identifier for the stack set operation.
--
-- 'callAs', 'importStacksToStackSet_callAs' - By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   For service managed stack sets, specify @DELEGATED_ADMIN@.
--
-- 'operationPreferences', 'importStacksToStackSet_operationPreferences' - Undocumented member.
--
-- 'stackSetName', 'importStacksToStackSet_stackSetName' - The name of the stack set. The name must be unique in the Region where
-- you create your stack set.
--
-- 'stackIds', 'importStacksToStackSet_stackIds' - The IDs of the stacks you are importing into a stack set. You import up
-- to 10 stacks per stack set at a time.
newImportStacksToStackSet ::
  -- | 'stackSetName'
  Prelude.Text ->
  ImportStacksToStackSet
newImportStacksToStackSet pStackSetName_ =
  ImportStacksToStackSet'
    { operationId =
        Prelude.Nothing,
      callAs = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      stackSetName = pStackSetName_,
      stackIds = Prelude.mempty
    }

-- | A unique, user defined, identifier for the stack set operation.
importStacksToStackSet_operationId :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe Prelude.Text)
importStacksToStackSet_operationId = Lens.lens (\ImportStacksToStackSet' {operationId} -> operationId) (\s@ImportStacksToStackSet' {} a -> s {operationId = a} :: ImportStacksToStackSet)

-- | By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   For service managed stack sets, specify @DELEGATED_ADMIN@.
importStacksToStackSet_callAs :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe CallAs)
importStacksToStackSet_callAs = Lens.lens (\ImportStacksToStackSet' {callAs} -> callAs) (\s@ImportStacksToStackSet' {} a -> s {callAs = a} :: ImportStacksToStackSet)

-- | Undocumented member.
importStacksToStackSet_operationPreferences :: Lens.Lens' ImportStacksToStackSet (Prelude.Maybe StackSetOperationPreferences)
importStacksToStackSet_operationPreferences = Lens.lens (\ImportStacksToStackSet' {operationPreferences} -> operationPreferences) (\s@ImportStacksToStackSet' {} a -> s {operationPreferences = a} :: ImportStacksToStackSet)

-- | The name of the stack set. The name must be unique in the Region where
-- you create your stack set.
importStacksToStackSet_stackSetName :: Lens.Lens' ImportStacksToStackSet Prelude.Text
importStacksToStackSet_stackSetName = Lens.lens (\ImportStacksToStackSet' {stackSetName} -> stackSetName) (\s@ImportStacksToStackSet' {} a -> s {stackSetName = a} :: ImportStacksToStackSet)

-- | The IDs of the stacks you are importing into a stack set. You import up
-- to 10 stacks per stack set at a time.
importStacksToStackSet_stackIds :: Lens.Lens' ImportStacksToStackSet [Prelude.Text]
importStacksToStackSet_stackIds = Lens.lens (\ImportStacksToStackSet' {stackIds} -> stackIds) (\s@ImportStacksToStackSet' {} a -> s {stackIds = a} :: ImportStacksToStackSet) Prelude.. Lens._Coerce

instance Core.AWSRequest ImportStacksToStackSet where
  type
    AWSResponse ImportStacksToStackSet =
      ImportStacksToStackSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ImportStacksToStackSetResult"
      ( \s h x ->
          ImportStacksToStackSetResponse'
            Prelude.<$> (x Core..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportStacksToStackSet

instance Prelude.NFData ImportStacksToStackSet

instance Core.ToHeaders ImportStacksToStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ImportStacksToStackSet where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportStacksToStackSet where
  toQuery ImportStacksToStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ImportStacksToStackSet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "OperationId" Core.=: operationId,
        "CallAs" Core.=: callAs,
        "OperationPreferences" Core.=: operationPreferences,
        "StackSetName" Core.=: stackSetName,
        "StackIds"
          Core.=: Core.toQueryList "member" stackIds
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
