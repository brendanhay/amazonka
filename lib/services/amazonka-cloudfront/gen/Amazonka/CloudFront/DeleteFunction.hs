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
-- Module      : Amazonka.CloudFront.DeleteFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CloudFront function.
--
-- You cannot delete a function if it’s associated with a cache behavior.
-- First, update your distributions to remove the function association from
-- all cache behaviors, then delete the function.
--
-- To delete a function, you must provide the function’s name and version
-- (@ETag@ value). To get these values, you can use @ListFunctions@ and
-- @DescribeFunction@.
module Amazonka.CloudFront.DeleteFunction
  ( -- * Creating a Request
    DeleteFunction (..),
    newDeleteFunction,

    -- * Request Lenses
    deleteFunction_ifMatch,
    deleteFunction_name,

    -- * Destructuring the Response
    DeleteFunctionResponse (..),
    newDeleteFunctionResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { -- | The current version (@ETag@ value) of the function that you are
    -- deleting, which you can get using @DescribeFunction@.
    ifMatch :: Prelude.Text,
    -- | The name of the function that you are deleting.
    name :: Prelude.Text
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
-- 'ifMatch', 'deleteFunction_ifMatch' - The current version (@ETag@ value) of the function that you are
-- deleting, which you can get using @DescribeFunction@.
--
-- 'name', 'deleteFunction_name' - The name of the function that you are deleting.
newDeleteFunction ::
  -- | 'ifMatch'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteFunction
newDeleteFunction pIfMatch_ pName_ =
  DeleteFunction' {ifMatch = pIfMatch_, name = pName_}

-- | The current version (@ETag@ value) of the function that you are
-- deleting, which you can get using @DescribeFunction@.
deleteFunction_ifMatch :: Lens.Lens' DeleteFunction Prelude.Text
deleteFunction_ifMatch = Lens.lens (\DeleteFunction' {ifMatch} -> ifMatch) (\s@DeleteFunction' {} a -> s {ifMatch = a} :: DeleteFunction)

-- | The name of the function that you are deleting.
deleteFunction_name :: Lens.Lens' DeleteFunction Prelude.Text
deleteFunction_name = Lens.lens (\DeleteFunction' {name} -> name) (\s@DeleteFunction' {} a -> s {name = a} :: DeleteFunction)

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
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteFunction where
  rnf DeleteFunction' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteFunction where
  toHeaders DeleteFunction' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteFunction where
  toPath DeleteFunction' {..} =
    Prelude.mconcat
      ["/2020-05-31/function/", Data.toBS name]

instance Data.ToQuery DeleteFunction where
  toQuery = Prelude.const Prelude.mempty

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
