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
-- Module      : Amazonka.SSM.DeleteParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a parameter from the system. After deleting a parameter, wait for
-- at least 30 seconds to create a parameter with the same name.
module Amazonka.SSM.DeleteParameter
  ( -- * Creating a Request
    DeleteParameter (..),
    newDeleteParameter,

    -- * Request Lenses
    deleteParameter_name,

    -- * Destructuring the Response
    DeleteParameterResponse (..),
    newDeleteParameterResponse,

    -- * Response Lenses
    deleteParameterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteParameter' smart constructor.
data DeleteParameter = DeleteParameter'
  { -- | The name of the parameter to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteParameter_name' - The name of the parameter to delete.
newDeleteParameter ::
  -- | 'name'
  Prelude.Text ->
  DeleteParameter
newDeleteParameter pName_ =
  DeleteParameter' {name = pName_}

-- | The name of the parameter to delete.
deleteParameter_name :: Lens.Lens' DeleteParameter Prelude.Text
deleteParameter_name = Lens.lens (\DeleteParameter' {name} -> name) (\s@DeleteParameter' {} a -> s {name = a} :: DeleteParameter)

instance Core.AWSRequest DeleteParameter where
  type
    AWSResponse DeleteParameter =
      DeleteParameterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteParameterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteParameter where
  hashWithSalt _salt DeleteParameter' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteParameter where
  rnf DeleteParameter' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DeleteParameter" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteParameter where
  toJSON DeleteParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteParameter where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteParameterResponse' smart constructor.
data DeleteParameterResponse = DeleteParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteParameterResponse_httpStatus' - The response's http status code.
newDeleteParameterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteParameterResponse
newDeleteParameterResponse pHttpStatus_ =
  DeleteParameterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteParameterResponse_httpStatus :: Lens.Lens' DeleteParameterResponse Prelude.Int
deleteParameterResponse_httpStatus = Lens.lens (\DeleteParameterResponse' {httpStatus} -> httpStatus) (\s@DeleteParameterResponse' {} a -> s {httpStatus = a} :: DeleteParameterResponse)

instance Prelude.NFData DeleteParameterResponse where
  rnf DeleteParameterResponse' {..} =
    Prelude.rnf httpStatus
