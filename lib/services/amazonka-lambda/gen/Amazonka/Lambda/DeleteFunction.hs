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
-- Module      : Amazonka.Lambda.DeleteFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function. To delete a specific function version, use
-- the @Qualifier@ parameter. Otherwise, all versions and aliases are
-- deleted.
--
-- To delete Lambda event source mappings that invoke a function, use
-- DeleteEventSourceMapping. For Amazon Web Services and resources that
-- invoke your function directly, delete the trigger in the service where
-- you originally configured it.
module Amazonka.Lambda.DeleteFunction
  ( -- * Creating a Request
    DeleteFunction (..),
    newDeleteFunction,

    -- * Request Lenses
    deleteFunction_qualifier,
    deleteFunction_functionName,

    -- * Destructuring the Response
    DeleteFunctionResponse (..),
    newDeleteFunctionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { -- | Specify a version to delete. You can\'t delete a version that an alias
    -- references.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function or version.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@ (name-only), @my-function:1@ (with
    --     version).
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'deleteFunction_qualifier' - Specify a version to delete. You can\'t delete a version that an alias
-- references.
--
-- 'functionName', 'deleteFunction_functionName' - The name of the Lambda function or version.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@ (name-only), @my-function:1@ (with
--     version).
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newDeleteFunction ::
  -- | 'functionName'
  Prelude.Text ->
  DeleteFunction
newDeleteFunction pFunctionName_ =
  DeleteFunction'
    { qualifier = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version to delete. You can\'t delete a version that an alias
-- references.
deleteFunction_qualifier :: Lens.Lens' DeleteFunction (Prelude.Maybe Prelude.Text)
deleteFunction_qualifier = Lens.lens (\DeleteFunction' {qualifier} -> qualifier) (\s@DeleteFunction' {} a -> s {qualifier = a} :: DeleteFunction)

-- | The name of the Lambda function or version.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@ (name-only), @my-function:1@ (with
--     version).
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
deleteFunction_functionName :: Lens.Lens' DeleteFunction Prelude.Text
deleteFunction_functionName = Lens.lens (\DeleteFunction' {functionName} -> functionName) (\s@DeleteFunction' {} a -> s {functionName = a} :: DeleteFunction)

instance Core.AWSRequest DeleteFunction where
  type
    AWSResponse DeleteFunction =
      DeleteFunctionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteFunctionResponse'

instance Prelude.Hashable DeleteFunction where
  hashWithSalt _salt DeleteFunction' {..} =
    _salt
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData DeleteFunction where
  rnf DeleteFunction' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders DeleteFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    Prelude.mconcat
      ["/2015-03-31/functions/", Data.toBS functionName]

instance Data.ToQuery DeleteFunction where
  toQuery DeleteFunction' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newDeleteFunctionResponse' smart constructor.
data DeleteFunctionResponse = DeleteFunctionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFunctionResponse ::
  DeleteFunctionResponse
newDeleteFunctionResponse = DeleteFunctionResponse'

instance Prelude.NFData DeleteFunctionResponse where
  rnf _ = ()
