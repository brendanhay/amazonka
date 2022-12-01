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
-- Module      : Amazonka.Schemas.DescribeCodeBinding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe the code binding URI.
module Amazonka.Schemas.DescribeCodeBinding
  ( -- * Creating a Request
    DescribeCodeBinding (..),
    newDescribeCodeBinding,

    -- * Request Lenses
    describeCodeBinding_schemaVersion,
    describeCodeBinding_registryName,
    describeCodeBinding_schemaName,
    describeCodeBinding_language,

    -- * Destructuring the Response
    DescribeCodeBindingResponse (..),
    newDescribeCodeBindingResponse,

    -- * Response Lenses
    describeCodeBindingResponse_creationDate,
    describeCodeBindingResponse_status,
    describeCodeBindingResponse_lastModified,
    describeCodeBindingResponse_schemaVersion,
    describeCodeBindingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newDescribeCodeBinding' smart constructor.
data DescribeCodeBinding = DescribeCodeBinding'
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
-- Create a value of 'DescribeCodeBinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersion', 'describeCodeBinding_schemaVersion' - Specifying this limits the results to only this schema version.
--
-- 'registryName', 'describeCodeBinding_registryName' - The name of the registry.
--
-- 'schemaName', 'describeCodeBinding_schemaName' - The name of the schema.
--
-- 'language', 'describeCodeBinding_language' - The language of the code binding.
newDescribeCodeBinding ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  -- | 'language'
  Prelude.Text ->
  DescribeCodeBinding
newDescribeCodeBinding
  pRegistryName_
  pSchemaName_
  pLanguage_ =
    DescribeCodeBinding'
      { schemaVersion =
          Prelude.Nothing,
        registryName = pRegistryName_,
        schemaName = pSchemaName_,
        language = pLanguage_
      }

-- | Specifying this limits the results to only this schema version.
describeCodeBinding_schemaVersion :: Lens.Lens' DescribeCodeBinding (Prelude.Maybe Prelude.Text)
describeCodeBinding_schemaVersion = Lens.lens (\DescribeCodeBinding' {schemaVersion} -> schemaVersion) (\s@DescribeCodeBinding' {} a -> s {schemaVersion = a} :: DescribeCodeBinding)

-- | The name of the registry.
describeCodeBinding_registryName :: Lens.Lens' DescribeCodeBinding Prelude.Text
describeCodeBinding_registryName = Lens.lens (\DescribeCodeBinding' {registryName} -> registryName) (\s@DescribeCodeBinding' {} a -> s {registryName = a} :: DescribeCodeBinding)

-- | The name of the schema.
describeCodeBinding_schemaName :: Lens.Lens' DescribeCodeBinding Prelude.Text
describeCodeBinding_schemaName = Lens.lens (\DescribeCodeBinding' {schemaName} -> schemaName) (\s@DescribeCodeBinding' {} a -> s {schemaName = a} :: DescribeCodeBinding)

-- | The language of the code binding.
describeCodeBinding_language :: Lens.Lens' DescribeCodeBinding Prelude.Text
describeCodeBinding_language = Lens.lens (\DescribeCodeBinding' {language} -> language) (\s@DescribeCodeBinding' {} a -> s {language = a} :: DescribeCodeBinding)

instance Core.AWSRequest DescribeCodeBinding where
  type
    AWSResponse DescribeCodeBinding =
      DescribeCodeBindingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeBindingResponse'
            Prelude.<$> (x Core..?> "CreationDate")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "LastModified")
            Prelude.<*> (x Core..?> "SchemaVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCodeBinding where
  hashWithSalt _salt DescribeCodeBinding' {..} =
    _salt `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` language

instance Prelude.NFData DescribeCodeBinding where
  rnf DescribeCodeBinding' {..} =
    Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf language

instance Core.ToHeaders DescribeCodeBinding where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeCodeBinding where
  toPath DescribeCodeBinding' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Core.toBS registryName,
        "/schemas/name/",
        Core.toBS schemaName,
        "/language/",
        Core.toBS language
      ]

instance Core.ToQuery DescribeCodeBinding where
  toQuery DescribeCodeBinding' {..} =
    Prelude.mconcat
      ["schemaVersion" Core.=: schemaVersion]

-- | /See:/ 'newDescribeCodeBindingResponse' smart constructor.
data DescribeCodeBindingResponse = DescribeCodeBindingResponse'
  { -- | The time and date that the code binding was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The current status of code binding generation.
    status :: Prelude.Maybe CodeGenerationStatus,
    -- | The date and time that code bindings were modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | The version number of the schema.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeBindingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'describeCodeBindingResponse_creationDate' - The time and date that the code binding was created.
--
-- 'status', 'describeCodeBindingResponse_status' - The current status of code binding generation.
--
-- 'lastModified', 'describeCodeBindingResponse_lastModified' - The date and time that code bindings were modified.
--
-- 'schemaVersion', 'describeCodeBindingResponse_schemaVersion' - The version number of the schema.
--
-- 'httpStatus', 'describeCodeBindingResponse_httpStatus' - The response's http status code.
newDescribeCodeBindingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCodeBindingResponse
newDescribeCodeBindingResponse pHttpStatus_ =
  DescribeCodeBindingResponse'
    { creationDate =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time and date that the code binding was created.
describeCodeBindingResponse_creationDate :: Lens.Lens' DescribeCodeBindingResponse (Prelude.Maybe Prelude.UTCTime)
describeCodeBindingResponse_creationDate = Lens.lens (\DescribeCodeBindingResponse' {creationDate} -> creationDate) (\s@DescribeCodeBindingResponse' {} a -> s {creationDate = a} :: DescribeCodeBindingResponse) Prelude.. Lens.mapping Core._Time

-- | The current status of code binding generation.
describeCodeBindingResponse_status :: Lens.Lens' DescribeCodeBindingResponse (Prelude.Maybe CodeGenerationStatus)
describeCodeBindingResponse_status = Lens.lens (\DescribeCodeBindingResponse' {status} -> status) (\s@DescribeCodeBindingResponse' {} a -> s {status = a} :: DescribeCodeBindingResponse)

-- | The date and time that code bindings were modified.
describeCodeBindingResponse_lastModified :: Lens.Lens' DescribeCodeBindingResponse (Prelude.Maybe Prelude.UTCTime)
describeCodeBindingResponse_lastModified = Lens.lens (\DescribeCodeBindingResponse' {lastModified} -> lastModified) (\s@DescribeCodeBindingResponse' {} a -> s {lastModified = a} :: DescribeCodeBindingResponse) Prelude.. Lens.mapping Core._Time

-- | The version number of the schema.
describeCodeBindingResponse_schemaVersion :: Lens.Lens' DescribeCodeBindingResponse (Prelude.Maybe Prelude.Text)
describeCodeBindingResponse_schemaVersion = Lens.lens (\DescribeCodeBindingResponse' {schemaVersion} -> schemaVersion) (\s@DescribeCodeBindingResponse' {} a -> s {schemaVersion = a} :: DescribeCodeBindingResponse)

-- | The response's http status code.
describeCodeBindingResponse_httpStatus :: Lens.Lens' DescribeCodeBindingResponse Prelude.Int
describeCodeBindingResponse_httpStatus = Lens.lens (\DescribeCodeBindingResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeBindingResponse' {} a -> s {httpStatus = a} :: DescribeCodeBindingResponse)

instance Prelude.NFData DescribeCodeBindingResponse where
  rnf DescribeCodeBindingResponse' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf httpStatus
