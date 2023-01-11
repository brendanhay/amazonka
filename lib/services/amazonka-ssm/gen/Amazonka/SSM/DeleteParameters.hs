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
-- Module      : Amazonka.SSM.DeleteParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a list of parameters. After deleting a parameter, wait for at
-- least 30 seconds to create a parameter with the same name.
module Amazonka.SSM.DeleteParameters
  ( -- * Creating a Request
    DeleteParameters (..),
    newDeleteParameters,

    -- * Request Lenses
    deleteParameters_names,

    -- * Destructuring the Response
    DeleteParametersResponse (..),
    newDeleteParametersResponse,

    -- * Response Lenses
    deleteParametersResponse_deletedParameters,
    deleteParametersResponse_invalidParameters,
    deleteParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteParameters' smart constructor.
data DeleteParameters = DeleteParameters'
  { -- | The names of the parameters to delete. After deleting a parameter, wait
    -- for at least 30 seconds to create a parameter with the same name.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'deleteParameters_names' - The names of the parameters to delete. After deleting a parameter, wait
-- for at least 30 seconds to create a parameter with the same name.
newDeleteParameters ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  DeleteParameters
newDeleteParameters pNames_ =
  DeleteParameters'
    { names =
        Lens.coerced Lens.# pNames_
    }

-- | The names of the parameters to delete. After deleting a parameter, wait
-- for at least 30 seconds to create a parameter with the same name.
deleteParameters_names :: Lens.Lens' DeleteParameters (Prelude.NonEmpty Prelude.Text)
deleteParameters_names = Lens.lens (\DeleteParameters' {names} -> names) (\s@DeleteParameters' {} a -> s {names = a} :: DeleteParameters) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteParameters where
  type
    AWSResponse DeleteParameters =
      DeleteParametersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParametersResponse'
            Prelude.<$> (x Data..?> "DeletedParameters")
            Prelude.<*> (x Data..?> "InvalidParameters")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteParameters where
  hashWithSalt _salt DeleteParameters' {..} =
    _salt `Prelude.hashWithSalt` names

instance Prelude.NFData DeleteParameters where
  rnf DeleteParameters' {..} = Prelude.rnf names

instance Data.ToHeaders DeleteParameters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DeleteParameters" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteParameters where
  toJSON DeleteParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Names" Data..= names)]
      )

instance Data.ToPath DeleteParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteParameters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteParametersResponse' smart constructor.
data DeleteParametersResponse = DeleteParametersResponse'
  { -- | The names of the deleted parameters.
    deletedParameters :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The names of parameters that weren\'t deleted because the parameters
    -- aren\'t valid.
    invalidParameters :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletedParameters', 'deleteParametersResponse_deletedParameters' - The names of the deleted parameters.
--
-- 'invalidParameters', 'deleteParametersResponse_invalidParameters' - The names of parameters that weren\'t deleted because the parameters
-- aren\'t valid.
--
-- 'httpStatus', 'deleteParametersResponse_httpStatus' - The response's http status code.
newDeleteParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteParametersResponse
newDeleteParametersResponse pHttpStatus_ =
  DeleteParametersResponse'
    { deletedParameters =
        Prelude.Nothing,
      invalidParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the deleted parameters.
deleteParametersResponse_deletedParameters :: Lens.Lens' DeleteParametersResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteParametersResponse_deletedParameters = Lens.lens (\DeleteParametersResponse' {deletedParameters} -> deletedParameters) (\s@DeleteParametersResponse' {} a -> s {deletedParameters = a} :: DeleteParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The names of parameters that weren\'t deleted because the parameters
-- aren\'t valid.
deleteParametersResponse_invalidParameters :: Lens.Lens' DeleteParametersResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteParametersResponse_invalidParameters = Lens.lens (\DeleteParametersResponse' {invalidParameters} -> invalidParameters) (\s@DeleteParametersResponse' {} a -> s {invalidParameters = a} :: DeleteParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteParametersResponse_httpStatus :: Lens.Lens' DeleteParametersResponse Prelude.Int
deleteParametersResponse_httpStatus = Lens.lens (\DeleteParametersResponse' {httpStatus} -> httpStatus) (\s@DeleteParametersResponse' {} a -> s {httpStatus = a} :: DeleteParametersResponse)

instance Prelude.NFData DeleteParametersResponse where
  rnf DeleteParametersResponse' {..} =
    Prelude.rnf deletedParameters
      `Prelude.seq` Prelude.rnf invalidParameters
      `Prelude.seq` Prelude.rnf httpStatus
