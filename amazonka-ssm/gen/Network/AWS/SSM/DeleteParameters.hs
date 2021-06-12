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
-- Module      : Network.AWS.SSM.DeleteParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a list of parameters.
module Network.AWS.SSM.DeleteParameters
  ( -- * Creating a Request
    DeleteParameters (..),
    newDeleteParameters,

    -- * Request Lenses
    deleteParameters_names,

    -- * Destructuring the Response
    DeleteParametersResponse (..),
    newDeleteParametersResponse,

    -- * Response Lenses
    deleteParametersResponse_invalidParameters,
    deleteParametersResponse_deletedParameters,
    deleteParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteParameters' smart constructor.
data DeleteParameters = DeleteParameters'
  { -- | The names of the parameters to delete.
    names :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'deleteParameters_names' - The names of the parameters to delete.
newDeleteParameters ::
  -- | 'names'
  Core.NonEmpty Core.Text ->
  DeleteParameters
newDeleteParameters pNames_ =
  DeleteParameters'
    { names =
        Lens._Coerce Lens.# pNames_
    }

-- | The names of the parameters to delete.
deleteParameters_names :: Lens.Lens' DeleteParameters (Core.NonEmpty Core.Text)
deleteParameters_names = Lens.lens (\DeleteParameters' {names} -> names) (\s@DeleteParameters' {} a -> s {names = a} :: DeleteParameters) Core.. Lens._Coerce

instance Core.AWSRequest DeleteParameters where
  type
    AWSResponse DeleteParameters =
      DeleteParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParametersResponse'
            Core.<$> (x Core..?> "InvalidParameters")
            Core.<*> (x Core..?> "DeletedParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteParameters

instance Core.NFData DeleteParameters

instance Core.ToHeaders DeleteParameters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DeleteParameters" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteParameters where
  toJSON DeleteParameters' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Names" Core..= names)])

instance Core.ToPath DeleteParameters where
  toPath = Core.const "/"

instance Core.ToQuery DeleteParameters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteParametersResponse' smart constructor.
data DeleteParametersResponse = DeleteParametersResponse'
  { -- | The names of parameters that weren\'t deleted because the parameters are
    -- not valid.
    invalidParameters :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The names of the deleted parameters.
    deletedParameters :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidParameters', 'deleteParametersResponse_invalidParameters' - The names of parameters that weren\'t deleted because the parameters are
-- not valid.
--
-- 'deletedParameters', 'deleteParametersResponse_deletedParameters' - The names of the deleted parameters.
--
-- 'httpStatus', 'deleteParametersResponse_httpStatus' - The response's http status code.
newDeleteParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteParametersResponse
newDeleteParametersResponse pHttpStatus_ =
  DeleteParametersResponse'
    { invalidParameters =
        Core.Nothing,
      deletedParameters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of parameters that weren\'t deleted because the parameters are
-- not valid.
deleteParametersResponse_invalidParameters :: Lens.Lens' DeleteParametersResponse (Core.Maybe (Core.NonEmpty Core.Text))
deleteParametersResponse_invalidParameters = Lens.lens (\DeleteParametersResponse' {invalidParameters} -> invalidParameters) (\s@DeleteParametersResponse' {} a -> s {invalidParameters = a} :: DeleteParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The names of the deleted parameters.
deleteParametersResponse_deletedParameters :: Lens.Lens' DeleteParametersResponse (Core.Maybe (Core.NonEmpty Core.Text))
deleteParametersResponse_deletedParameters = Lens.lens (\DeleteParametersResponse' {deletedParameters} -> deletedParameters) (\s@DeleteParametersResponse' {} a -> s {deletedParameters = a} :: DeleteParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteParametersResponse_httpStatus :: Lens.Lens' DeleteParametersResponse Core.Int
deleteParametersResponse_httpStatus = Lens.lens (\DeleteParametersResponse' {httpStatus} -> httpStatus) (\s@DeleteParametersResponse' {} a -> s {httpStatus = a} :: DeleteParametersResponse)

instance Core.NFData DeleteParametersResponse
