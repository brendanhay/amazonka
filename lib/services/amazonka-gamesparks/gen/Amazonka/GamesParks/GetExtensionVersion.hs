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
-- Module      : Amazonka.GamesParks.GetExtensionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specified extension version.
module Amazonka.GamesParks.GetExtensionVersion
  ( -- * Creating a Request
    GetExtensionVersion (..),
    newGetExtensionVersion,

    -- * Request Lenses
    getExtensionVersion_extensionVersion,
    getExtensionVersion_name,
    getExtensionVersion_namespace,

    -- * Destructuring the Response
    GetExtensionVersionResponse (..),
    newGetExtensionVersionResponse,

    -- * Response Lenses
    getExtensionVersionResponse_extensionVersion,
    getExtensionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExtensionVersion' smart constructor.
data GetExtensionVersion = GetExtensionVersion'
  { -- | The version of the extension.
    extensionVersion :: Prelude.Text,
    -- | The name of the extension.
    name :: Prelude.Text,
    -- | The namespace (qualifier) of the extension.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExtensionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionVersion', 'getExtensionVersion_extensionVersion' - The version of the extension.
--
-- 'name', 'getExtensionVersion_name' - The name of the extension.
--
-- 'namespace', 'getExtensionVersion_namespace' - The namespace (qualifier) of the extension.
newGetExtensionVersion ::
  -- | 'extensionVersion'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  GetExtensionVersion
newGetExtensionVersion
  pExtensionVersion_
  pName_
  pNamespace_ =
    GetExtensionVersion'
      { extensionVersion =
          pExtensionVersion_,
        name = pName_,
        namespace = pNamespace_
      }

-- | The version of the extension.
getExtensionVersion_extensionVersion :: Lens.Lens' GetExtensionVersion Prelude.Text
getExtensionVersion_extensionVersion = Lens.lens (\GetExtensionVersion' {extensionVersion} -> extensionVersion) (\s@GetExtensionVersion' {} a -> s {extensionVersion = a} :: GetExtensionVersion)

-- | The name of the extension.
getExtensionVersion_name :: Lens.Lens' GetExtensionVersion Prelude.Text
getExtensionVersion_name = Lens.lens (\GetExtensionVersion' {name} -> name) (\s@GetExtensionVersion' {} a -> s {name = a} :: GetExtensionVersion)

-- | The namespace (qualifier) of the extension.
getExtensionVersion_namespace :: Lens.Lens' GetExtensionVersion Prelude.Text
getExtensionVersion_namespace = Lens.lens (\GetExtensionVersion' {namespace} -> namespace) (\s@GetExtensionVersion' {} a -> s {namespace = a} :: GetExtensionVersion)

instance Core.AWSRequest GetExtensionVersion where
  type
    AWSResponse GetExtensionVersion =
      GetExtensionVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExtensionVersionResponse'
            Prelude.<$> (x Data..?> "ExtensionVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExtensionVersion where
  hashWithSalt _salt GetExtensionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` extensionVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData GetExtensionVersion where
  rnf GetExtensionVersion' {..} =
    Prelude.rnf extensionVersion `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf namespace

instance Data.ToHeaders GetExtensionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExtensionVersion where
  toPath GetExtensionVersion' {..} =
    Prelude.mconcat
      [ "/extension/",
        Data.toBS namespace,
        "/",
        Data.toBS name,
        "/version/",
        Data.toBS extensionVersion
      ]

instance Data.ToQuery GetExtensionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExtensionVersionResponse' smart constructor.
data GetExtensionVersionResponse = GetExtensionVersionResponse'
  { -- | The version of the extension.
    extensionVersion :: Prelude.Maybe ExtensionVersionDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExtensionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionVersion', 'getExtensionVersionResponse_extensionVersion' - The version of the extension.
--
-- 'httpStatus', 'getExtensionVersionResponse_httpStatus' - The response's http status code.
newGetExtensionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExtensionVersionResponse
newGetExtensionVersionResponse pHttpStatus_ =
  GetExtensionVersionResponse'
    { extensionVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the extension.
getExtensionVersionResponse_extensionVersion :: Lens.Lens' GetExtensionVersionResponse (Prelude.Maybe ExtensionVersionDetails)
getExtensionVersionResponse_extensionVersion = Lens.lens (\GetExtensionVersionResponse' {extensionVersion} -> extensionVersion) (\s@GetExtensionVersionResponse' {} a -> s {extensionVersion = a} :: GetExtensionVersionResponse)

-- | The response's http status code.
getExtensionVersionResponse_httpStatus :: Lens.Lens' GetExtensionVersionResponse Prelude.Int
getExtensionVersionResponse_httpStatus = Lens.lens (\GetExtensionVersionResponse' {httpStatus} -> httpStatus) (\s@GetExtensionVersionResponse' {} a -> s {httpStatus = a} :: GetExtensionVersionResponse)

instance Prelude.NFData GetExtensionVersionResponse where
  rnf GetExtensionVersionResponse' {..} =
    Prelude.rnf extensionVersion `Prelude.seq`
      Prelude.rnf httpStatus
