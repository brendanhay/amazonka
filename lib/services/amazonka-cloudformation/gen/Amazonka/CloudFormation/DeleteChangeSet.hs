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
-- Module      : Amazonka.CloudFormation.DeleteChangeSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified change set. Deleting change sets ensures that no
-- one executes the wrong change set.
--
-- If the call successfully completes, CloudFormation successfully deleted
-- the change set.
--
-- If @IncludeNestedStacks@ specifies @True@ during the creation of the
-- nested change set, then @DeleteChangeSet@ will delete all change sets
-- that belong to the stacks hierarchy and will also delete all change sets
-- for nested stacks with the status of @REVIEW_IN_PROGRESS@.
module Amazonka.CloudFormation.DeleteChangeSet
  ( -- * Creating a Request
    DeleteChangeSet (..),
    newDeleteChangeSet,

    -- * Request Lenses
    deleteChangeSet_stackName,
    deleteChangeSet_changeSetName,

    -- * Destructuring the Response
    DeleteChangeSetResponse (..),
    newDeleteChangeSetResponse,

    -- * Response Lenses
    deleteChangeSetResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeleteChangeSet action.
--
-- /See:/ 'newDeleteChangeSet' smart constructor.
data DeleteChangeSet = DeleteChangeSet'
  { -- | If you specified the name of a change set to delete, specify the stack
    -- name or Amazon Resource Name (ARN) that\'s associated with it.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the change set that you want
    -- to delete.
    changeSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'deleteChangeSet_stackName' - If you specified the name of a change set to delete, specify the stack
-- name or Amazon Resource Name (ARN) that\'s associated with it.
--
-- 'changeSetName', 'deleteChangeSet_changeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want
-- to delete.
newDeleteChangeSet ::
  -- | 'changeSetName'
  Prelude.Text ->
  DeleteChangeSet
newDeleteChangeSet pChangeSetName_ =
  DeleteChangeSet'
    { stackName = Prelude.Nothing,
      changeSetName = pChangeSetName_
    }

-- | If you specified the name of a change set to delete, specify the stack
-- name or Amazon Resource Name (ARN) that\'s associated with it.
deleteChangeSet_stackName :: Lens.Lens' DeleteChangeSet (Prelude.Maybe Prelude.Text)
deleteChangeSet_stackName = Lens.lens (\DeleteChangeSet' {stackName} -> stackName) (\s@DeleteChangeSet' {} a -> s {stackName = a} :: DeleteChangeSet)

-- | The name or Amazon Resource Name (ARN) of the change set that you want
-- to delete.
deleteChangeSet_changeSetName :: Lens.Lens' DeleteChangeSet Prelude.Text
deleteChangeSet_changeSetName = Lens.lens (\DeleteChangeSet' {changeSetName} -> changeSetName) (\s@DeleteChangeSet' {} a -> s {changeSetName = a} :: DeleteChangeSet)

instance Core.AWSRequest DeleteChangeSet where
  type
    AWSResponse DeleteChangeSet =
      DeleteChangeSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteChangeSetResult"
      ( \s h x ->
          DeleteChangeSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChangeSet where
  hashWithSalt _salt DeleteChangeSet' {..} =
    _salt
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` changeSetName

instance Prelude.NFData DeleteChangeSet where
  rnf DeleteChangeSet' {..} =
    Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf changeSetName

instance Data.ToHeaders DeleteChangeSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteChangeSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteChangeSet where
  toQuery DeleteChangeSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteChangeSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Data.=: stackName,
        "ChangeSetName" Data.=: changeSetName
      ]

-- | The output for the DeleteChangeSet action.
--
-- /See:/ 'newDeleteChangeSetResponse' smart constructor.
data DeleteChangeSetResponse = DeleteChangeSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteChangeSetResponse_httpStatus' - The response's http status code.
newDeleteChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChangeSetResponse
newDeleteChangeSetResponse pHttpStatus_ =
  DeleteChangeSetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteChangeSetResponse_httpStatus :: Lens.Lens' DeleteChangeSetResponse Prelude.Int
deleteChangeSetResponse_httpStatus = Lens.lens (\DeleteChangeSetResponse' {httpStatus} -> httpStatus) (\s@DeleteChangeSetResponse' {} a -> s {httpStatus = a} :: DeleteChangeSetResponse)

instance Prelude.NFData DeleteChangeSetResponse where
  rnf DeleteChangeSetResponse' {..} =
    Prelude.rnf httpStatus
