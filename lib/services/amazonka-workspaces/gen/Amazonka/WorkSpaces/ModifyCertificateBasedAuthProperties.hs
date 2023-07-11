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
-- Module      : Amazonka.WorkSpaces.ModifyCertificateBasedAuthProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of the certificate-based authentication you want
-- to use with your WorkSpaces.
module Amazonka.WorkSpaces.ModifyCertificateBasedAuthProperties
  ( -- * Creating a Request
    ModifyCertificateBasedAuthProperties (..),
    newModifyCertificateBasedAuthProperties,

    -- * Request Lenses
    modifyCertificateBasedAuthProperties_certificateBasedAuthProperties,
    modifyCertificateBasedAuthProperties_propertiesToDelete,
    modifyCertificateBasedAuthProperties_resourceId,

    -- * Destructuring the Response
    ModifyCertificateBasedAuthPropertiesResponse (..),
    newModifyCertificateBasedAuthPropertiesResponse,

    -- * Response Lenses
    modifyCertificateBasedAuthPropertiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newModifyCertificateBasedAuthProperties' smart constructor.
data ModifyCertificateBasedAuthProperties = ModifyCertificateBasedAuthProperties'
  { -- | The properties of the certificate-based authentication.
    certificateBasedAuthProperties :: Prelude.Maybe CertificateBasedAuthProperties,
    -- | The properties of the certificate-based authentication you want to
    -- delete.
    propertiesToDelete :: Prelude.Maybe [DeletableCertificateBasedAuthProperty],
    -- | The resource identifiers, in the form of directory IDs.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCertificateBasedAuthProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateBasedAuthProperties', 'modifyCertificateBasedAuthProperties_certificateBasedAuthProperties' - The properties of the certificate-based authentication.
--
-- 'propertiesToDelete', 'modifyCertificateBasedAuthProperties_propertiesToDelete' - The properties of the certificate-based authentication you want to
-- delete.
--
-- 'resourceId', 'modifyCertificateBasedAuthProperties_resourceId' - The resource identifiers, in the form of directory IDs.
newModifyCertificateBasedAuthProperties ::
  -- | 'resourceId'
  Prelude.Text ->
  ModifyCertificateBasedAuthProperties
newModifyCertificateBasedAuthProperties pResourceId_ =
  ModifyCertificateBasedAuthProperties'
    { certificateBasedAuthProperties =
        Prelude.Nothing,
      propertiesToDelete = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The properties of the certificate-based authentication.
modifyCertificateBasedAuthProperties_certificateBasedAuthProperties :: Lens.Lens' ModifyCertificateBasedAuthProperties (Prelude.Maybe CertificateBasedAuthProperties)
modifyCertificateBasedAuthProperties_certificateBasedAuthProperties = Lens.lens (\ModifyCertificateBasedAuthProperties' {certificateBasedAuthProperties} -> certificateBasedAuthProperties) (\s@ModifyCertificateBasedAuthProperties' {} a -> s {certificateBasedAuthProperties = a} :: ModifyCertificateBasedAuthProperties)

-- | The properties of the certificate-based authentication you want to
-- delete.
modifyCertificateBasedAuthProperties_propertiesToDelete :: Lens.Lens' ModifyCertificateBasedAuthProperties (Prelude.Maybe [DeletableCertificateBasedAuthProperty])
modifyCertificateBasedAuthProperties_propertiesToDelete = Lens.lens (\ModifyCertificateBasedAuthProperties' {propertiesToDelete} -> propertiesToDelete) (\s@ModifyCertificateBasedAuthProperties' {} a -> s {propertiesToDelete = a} :: ModifyCertificateBasedAuthProperties) Prelude.. Lens.mapping Lens.coerced

-- | The resource identifiers, in the form of directory IDs.
modifyCertificateBasedAuthProperties_resourceId :: Lens.Lens' ModifyCertificateBasedAuthProperties Prelude.Text
modifyCertificateBasedAuthProperties_resourceId = Lens.lens (\ModifyCertificateBasedAuthProperties' {resourceId} -> resourceId) (\s@ModifyCertificateBasedAuthProperties' {} a -> s {resourceId = a} :: ModifyCertificateBasedAuthProperties)

instance
  Core.AWSRequest
    ModifyCertificateBasedAuthProperties
  where
  type
    AWSResponse ModifyCertificateBasedAuthProperties =
      ModifyCertificateBasedAuthPropertiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyCertificateBasedAuthPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyCertificateBasedAuthProperties
  where
  hashWithSalt
    _salt
    ModifyCertificateBasedAuthProperties' {..} =
      _salt
        `Prelude.hashWithSalt` certificateBasedAuthProperties
        `Prelude.hashWithSalt` propertiesToDelete
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    ModifyCertificateBasedAuthProperties
  where
  rnf ModifyCertificateBasedAuthProperties' {..} =
    Prelude.rnf certificateBasedAuthProperties
      `Prelude.seq` Prelude.rnf propertiesToDelete
      `Prelude.seq` Prelude.rnf resourceId

instance
  Data.ToHeaders
    ModifyCertificateBasedAuthProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.ModifyCertificateBasedAuthProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ModifyCertificateBasedAuthProperties
  where
  toJSON ModifyCertificateBasedAuthProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateBasedAuthProperties" Data..=)
              Prelude.<$> certificateBasedAuthProperties,
            ("PropertiesToDelete" Data..=)
              Prelude.<$> propertiesToDelete,
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance
  Data.ToPath
    ModifyCertificateBasedAuthProperties
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyCertificateBasedAuthProperties
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyCertificateBasedAuthPropertiesResponse' smart constructor.
data ModifyCertificateBasedAuthPropertiesResponse = ModifyCertificateBasedAuthPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCertificateBasedAuthPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyCertificateBasedAuthPropertiesResponse_httpStatus' - The response's http status code.
newModifyCertificateBasedAuthPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCertificateBasedAuthPropertiesResponse
newModifyCertificateBasedAuthPropertiesResponse
  pHttpStatus_ =
    ModifyCertificateBasedAuthPropertiesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
modifyCertificateBasedAuthPropertiesResponse_httpStatus :: Lens.Lens' ModifyCertificateBasedAuthPropertiesResponse Prelude.Int
modifyCertificateBasedAuthPropertiesResponse_httpStatus = Lens.lens (\ModifyCertificateBasedAuthPropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyCertificateBasedAuthPropertiesResponse' {} a -> s {httpStatus = a} :: ModifyCertificateBasedAuthPropertiesResponse)

instance
  Prelude.NFData
    ModifyCertificateBasedAuthPropertiesResponse
  where
  rnf ModifyCertificateBasedAuthPropertiesResponse' {..} =
    Prelude.rnf httpStatus
