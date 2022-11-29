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
-- Module      : Amazonka.Schemas.GetCodeBindingSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the code binding source URI.
module Amazonka.Schemas.GetCodeBindingSource
  ( -- * Creating a Request
    GetCodeBindingSource (..),
    newGetCodeBindingSource,

    -- * Request Lenses
    getCodeBindingSource_schemaVersion,
    getCodeBindingSource_registryName,
    getCodeBindingSource_schemaName,
    getCodeBindingSource_language,

    -- * Destructuring the Response
    GetCodeBindingSourceResponse (..),
    newGetCodeBindingSourceResponse,

    -- * Response Lenses
    getCodeBindingSourceResponse_body,
    getCodeBindingSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newGetCodeBindingSource' smart constructor.
data GetCodeBindingSource = GetCodeBindingSource'
  { -- | Specifying this limits the results to only this schema version.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Text,
    -- | The language of the code binding.
    language :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCodeBindingSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersion', 'getCodeBindingSource_schemaVersion' - Specifying this limits the results to only this schema version.
--
-- 'registryName', 'getCodeBindingSource_registryName' - The name of the registry.
--
-- 'schemaName', 'getCodeBindingSource_schemaName' - The name of the schema.
--
-- 'language', 'getCodeBindingSource_language' - The language of the code binding.
newGetCodeBindingSource ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  -- | 'language'
  Prelude.Text ->
  GetCodeBindingSource
newGetCodeBindingSource
  pRegistryName_
  pSchemaName_
  pLanguage_ =
    GetCodeBindingSource'
      { schemaVersion =
          Prelude.Nothing,
        registryName = pRegistryName_,
        schemaName = pSchemaName_,
        language = pLanguage_
      }

-- | Specifying this limits the results to only this schema version.
getCodeBindingSource_schemaVersion :: Lens.Lens' GetCodeBindingSource (Prelude.Maybe Prelude.Text)
getCodeBindingSource_schemaVersion = Lens.lens (\GetCodeBindingSource' {schemaVersion} -> schemaVersion) (\s@GetCodeBindingSource' {} a -> s {schemaVersion = a} :: GetCodeBindingSource)

-- | The name of the registry.
getCodeBindingSource_registryName :: Lens.Lens' GetCodeBindingSource Prelude.Text
getCodeBindingSource_registryName = Lens.lens (\GetCodeBindingSource' {registryName} -> registryName) (\s@GetCodeBindingSource' {} a -> s {registryName = a} :: GetCodeBindingSource)

-- | The name of the schema.
getCodeBindingSource_schemaName :: Lens.Lens' GetCodeBindingSource Prelude.Text
getCodeBindingSource_schemaName = Lens.lens (\GetCodeBindingSource' {schemaName} -> schemaName) (\s@GetCodeBindingSource' {} a -> s {schemaName = a} :: GetCodeBindingSource)

-- | The language of the code binding.
getCodeBindingSource_language :: Lens.Lens' GetCodeBindingSource Prelude.Text
getCodeBindingSource_language = Lens.lens (\GetCodeBindingSource' {language} -> language) (\s@GetCodeBindingSource' {} a -> s {language = a} :: GetCodeBindingSource)

instance Core.AWSRequest GetCodeBindingSource where
  type
    AWSResponse GetCodeBindingSource =
      GetCodeBindingSourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetCodeBindingSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCodeBindingSource where
  hashWithSalt _salt GetCodeBindingSource' {..} =
    _salt `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` language

instance Prelude.NFData GetCodeBindingSource where
  rnf GetCodeBindingSource' {..} =
    Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf language

instance Core.ToHeaders GetCodeBindingSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCodeBindingSource where
  toPath GetCodeBindingSource' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Core.toBS registryName,
        "/schemas/name/",
        Core.toBS schemaName,
        "/language/",
        Core.toBS language,
        "/source"
      ]

instance Core.ToQuery GetCodeBindingSource where
  toQuery GetCodeBindingSource' {..} =
    Prelude.mconcat
      ["schemaVersion" Core.=: schemaVersion]

-- | /See:/ 'newGetCodeBindingSourceResponse' smart constructor.
data GetCodeBindingSourceResponse = GetCodeBindingSourceResponse'
  { body :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCodeBindingSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'getCodeBindingSourceResponse_body' - Undocumented member.
--
-- 'httpStatus', 'getCodeBindingSourceResponse_httpStatus' - The response's http status code.
newGetCodeBindingSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCodeBindingSourceResponse
newGetCodeBindingSourceResponse pHttpStatus_ =
  GetCodeBindingSourceResponse'
    { body =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getCodeBindingSourceResponse_body :: Lens.Lens' GetCodeBindingSourceResponse (Prelude.Maybe Prelude.ByteString)
getCodeBindingSourceResponse_body = Lens.lens (\GetCodeBindingSourceResponse' {body} -> body) (\s@GetCodeBindingSourceResponse' {} a -> s {body = a} :: GetCodeBindingSourceResponse)

-- | The response's http status code.
getCodeBindingSourceResponse_httpStatus :: Lens.Lens' GetCodeBindingSourceResponse Prelude.Int
getCodeBindingSourceResponse_httpStatus = Lens.lens (\GetCodeBindingSourceResponse' {httpStatus} -> httpStatus) (\s@GetCodeBindingSourceResponse' {} a -> s {httpStatus = a} :: GetCodeBindingSourceResponse)

instance Prelude.NFData GetCodeBindingSourceResponse where
  rnf GetCodeBindingSourceResponse' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf httpStatus
