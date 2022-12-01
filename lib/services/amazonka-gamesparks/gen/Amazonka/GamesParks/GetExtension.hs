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
-- Module      : Amazonka.GamesParks.GetExtension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specified extension.
module Amazonka.GamesParks.GetExtension
  ( -- * Creating a Request
    GetExtension (..),
    newGetExtension,

    -- * Request Lenses
    getExtension_name,
    getExtension_namespace,

    -- * Destructuring the Response
    GetExtensionResponse (..),
    newGetExtensionResponse,

    -- * Response Lenses
    getExtensionResponse_extension,
    getExtensionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExtension' smart constructor.
data GetExtension = GetExtension'
  { -- | The name of the extension.
    name :: Prelude.Text,
    -- | The namespace (qualifier) of the extension.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getExtension_name' - The name of the extension.
--
-- 'namespace', 'getExtension_namespace' - The namespace (qualifier) of the extension.
newGetExtension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  GetExtension
newGetExtension pName_ pNamespace_ =
  GetExtension'
    { name = pName_,
      namespace = pNamespace_
    }

-- | The name of the extension.
getExtension_name :: Lens.Lens' GetExtension Prelude.Text
getExtension_name = Lens.lens (\GetExtension' {name} -> name) (\s@GetExtension' {} a -> s {name = a} :: GetExtension)

-- | The namespace (qualifier) of the extension.
getExtension_namespace :: Lens.Lens' GetExtension Prelude.Text
getExtension_namespace = Lens.lens (\GetExtension' {namespace} -> namespace) (\s@GetExtension' {} a -> s {namespace = a} :: GetExtension)

instance Core.AWSRequest GetExtension where
  type AWSResponse GetExtension = GetExtensionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExtensionResponse'
            Prelude.<$> (x Core..?> "Extension")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExtension where
  hashWithSalt _salt GetExtension' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData GetExtension where
  rnf GetExtension' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders GetExtension where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetExtension where
  toPath GetExtension' {..} =
    Prelude.mconcat
      [ "/extension/",
        Core.toBS namespace,
        "/",
        Core.toBS name
      ]

instance Core.ToQuery GetExtension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExtensionResponse' smart constructor.
data GetExtensionResponse = GetExtensionResponse'
  { -- | Details about the extension.
    extension :: Prelude.Maybe ExtensionDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExtensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extension', 'getExtensionResponse_extension' - Details about the extension.
--
-- 'httpStatus', 'getExtensionResponse_httpStatus' - The response's http status code.
newGetExtensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExtensionResponse
newGetExtensionResponse pHttpStatus_ =
  GetExtensionResponse'
    { extension = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the extension.
getExtensionResponse_extension :: Lens.Lens' GetExtensionResponse (Prelude.Maybe ExtensionDetails)
getExtensionResponse_extension = Lens.lens (\GetExtensionResponse' {extension} -> extension) (\s@GetExtensionResponse' {} a -> s {extension = a} :: GetExtensionResponse)

-- | The response's http status code.
getExtensionResponse_httpStatus :: Lens.Lens' GetExtensionResponse Prelude.Int
getExtensionResponse_httpStatus = Lens.lens (\GetExtensionResponse' {httpStatus} -> httpStatus) (\s@GetExtensionResponse' {} a -> s {httpStatus = a} :: GetExtensionResponse)

instance Prelude.NFData GetExtensionResponse where
  rnf GetExtensionResponse' {..} =
    Prelude.rnf extension
      `Prelude.seq` Prelude.rnf httpStatus
