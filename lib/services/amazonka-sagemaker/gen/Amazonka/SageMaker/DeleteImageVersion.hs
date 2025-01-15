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
-- Module      : Amazonka.SageMaker.DeleteImageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of a SageMaker image. The container image the version
-- represents isn\'t deleted.
module Amazonka.SageMaker.DeleteImageVersion
  ( -- * Creating a Request
    DeleteImageVersion (..),
    newDeleteImageVersion,

    -- * Request Lenses
    deleteImageVersion_alias,
    deleteImageVersion_version,
    deleteImageVersion_imageName,

    -- * Destructuring the Response
    DeleteImageVersionResponse (..),
    newDeleteImageVersionResponse,

    -- * Response Lenses
    deleteImageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteImageVersion' smart constructor.
data DeleteImageVersion = DeleteImageVersion'
  { -- | The alias of the image to delete.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The version to delete.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the image to delete.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'deleteImageVersion_alias' - The alias of the image to delete.
--
-- 'version', 'deleteImageVersion_version' - The version to delete.
--
-- 'imageName', 'deleteImageVersion_imageName' - The name of the image to delete.
newDeleteImageVersion ::
  -- | 'imageName'
  Prelude.Text ->
  DeleteImageVersion
newDeleteImageVersion pImageName_ =
  DeleteImageVersion'
    { alias = Prelude.Nothing,
      version = Prelude.Nothing,
      imageName = pImageName_
    }

-- | The alias of the image to delete.
deleteImageVersion_alias :: Lens.Lens' DeleteImageVersion (Prelude.Maybe Prelude.Text)
deleteImageVersion_alias = Lens.lens (\DeleteImageVersion' {alias} -> alias) (\s@DeleteImageVersion' {} a -> s {alias = a} :: DeleteImageVersion)

-- | The version to delete.
deleteImageVersion_version :: Lens.Lens' DeleteImageVersion (Prelude.Maybe Prelude.Natural)
deleteImageVersion_version = Lens.lens (\DeleteImageVersion' {version} -> version) (\s@DeleteImageVersion' {} a -> s {version = a} :: DeleteImageVersion)

-- | The name of the image to delete.
deleteImageVersion_imageName :: Lens.Lens' DeleteImageVersion Prelude.Text
deleteImageVersion_imageName = Lens.lens (\DeleteImageVersion' {imageName} -> imageName) (\s@DeleteImageVersion' {} a -> s {imageName = a} :: DeleteImageVersion)

instance Core.AWSRequest DeleteImageVersion where
  type
    AWSResponse DeleteImageVersion =
      DeleteImageVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImageVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImageVersion where
  hashWithSalt _salt DeleteImageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` imageName

instance Prelude.NFData DeleteImageVersion where
  rnf DeleteImageVersion' {..} =
    Prelude.rnf alias `Prelude.seq`
      Prelude.rnf version `Prelude.seq`
        Prelude.rnf imageName

instance Data.ToHeaders DeleteImageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteImageVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteImageVersion where
  toJSON DeleteImageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Alias" Data..=) Prelude.<$> alias,
            ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("ImageName" Data..= imageName)
          ]
      )

instance Data.ToPath DeleteImageVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImageVersionResponse' smart constructor.
data DeleteImageVersionResponse = DeleteImageVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteImageVersionResponse_httpStatus' - The response's http status code.
newDeleteImageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImageVersionResponse
newDeleteImageVersionResponse pHttpStatus_ =
  DeleteImageVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteImageVersionResponse_httpStatus :: Lens.Lens' DeleteImageVersionResponse Prelude.Int
deleteImageVersionResponse_httpStatus = Lens.lens (\DeleteImageVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteImageVersionResponse' {} a -> s {httpStatus = a} :: DeleteImageVersionResponse)

instance Prelude.NFData DeleteImageVersionResponse where
  rnf DeleteImageVersionResponse' {..} =
    Prelude.rnf httpStatus
