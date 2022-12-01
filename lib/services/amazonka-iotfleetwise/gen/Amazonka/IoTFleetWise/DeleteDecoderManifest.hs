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
-- Module      : Amazonka.IoTFleetWise.DeleteDecoderManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a decoder manifest. You can\'t delete a decoder manifest if it
-- has vehicles associated with it.
--
-- If the decoder manifest is successfully deleted, Amazon Web Services IoT
-- FleetWise sends back an HTTP 200 response with an empty body.
module Amazonka.IoTFleetWise.DeleteDecoderManifest
  ( -- * Creating a Request
    DeleteDecoderManifest (..),
    newDeleteDecoderManifest,

    -- * Request Lenses
    deleteDecoderManifest_name,

    -- * Destructuring the Response
    DeleteDecoderManifestResponse (..),
    newDeleteDecoderManifestResponse,

    -- * Response Lenses
    deleteDecoderManifestResponse_httpStatus,
    deleteDecoderManifestResponse_name,
    deleteDecoderManifestResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDecoderManifest' smart constructor.
data DeleteDecoderManifest = DeleteDecoderManifest'
  { -- | The name of the decoder manifest to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDecoderManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteDecoderManifest_name' - The name of the decoder manifest to delete.
newDeleteDecoderManifest ::
  -- | 'name'
  Prelude.Text ->
  DeleteDecoderManifest
newDeleteDecoderManifest pName_ =
  DeleteDecoderManifest' {name = pName_}

-- | The name of the decoder manifest to delete.
deleteDecoderManifest_name :: Lens.Lens' DeleteDecoderManifest Prelude.Text
deleteDecoderManifest_name = Lens.lens (\DeleteDecoderManifest' {name} -> name) (\s@DeleteDecoderManifest' {} a -> s {name = a} :: DeleteDecoderManifest)

instance Core.AWSRequest DeleteDecoderManifest where
  type
    AWSResponse DeleteDecoderManifest =
      DeleteDecoderManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDecoderManifestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "arn")
      )

instance Prelude.Hashable DeleteDecoderManifest where
  hashWithSalt _salt DeleteDecoderManifest' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteDecoderManifest where
  rnf DeleteDecoderManifest' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteDecoderManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.DeleteDecoderManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDecoderManifest where
  toJSON DeleteDecoderManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath DeleteDecoderManifest where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDecoderManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDecoderManifestResponse' smart constructor.
data DeleteDecoderManifestResponse = DeleteDecoderManifestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the deleted decoder manifest.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deleted decoder manifest.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDecoderManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDecoderManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'deleteDecoderManifestResponse_name' - The name of the deleted decoder manifest.
--
-- 'arn', 'deleteDecoderManifestResponse_arn' - The Amazon Resource Name (ARN) of the deleted decoder manifest.
newDeleteDecoderManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  DeleteDecoderManifestResponse
newDeleteDecoderManifestResponse
  pHttpStatus_
  pName_
  pArn_ =
    DeleteDecoderManifestResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
deleteDecoderManifestResponse_httpStatus :: Lens.Lens' DeleteDecoderManifestResponse Prelude.Int
deleteDecoderManifestResponse_httpStatus = Lens.lens (\DeleteDecoderManifestResponse' {httpStatus} -> httpStatus) (\s@DeleteDecoderManifestResponse' {} a -> s {httpStatus = a} :: DeleteDecoderManifestResponse)

-- | The name of the deleted decoder manifest.
deleteDecoderManifestResponse_name :: Lens.Lens' DeleteDecoderManifestResponse Prelude.Text
deleteDecoderManifestResponse_name = Lens.lens (\DeleteDecoderManifestResponse' {name} -> name) (\s@DeleteDecoderManifestResponse' {} a -> s {name = a} :: DeleteDecoderManifestResponse)

-- | The Amazon Resource Name (ARN) of the deleted decoder manifest.
deleteDecoderManifestResponse_arn :: Lens.Lens' DeleteDecoderManifestResponse Prelude.Text
deleteDecoderManifestResponse_arn = Lens.lens (\DeleteDecoderManifestResponse' {arn} -> arn) (\s@DeleteDecoderManifestResponse' {} a -> s {arn = a} :: DeleteDecoderManifestResponse)

instance Prelude.NFData DeleteDecoderManifestResponse where
  rnf DeleteDecoderManifestResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
