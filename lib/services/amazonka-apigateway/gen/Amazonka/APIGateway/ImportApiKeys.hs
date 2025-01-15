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
-- Module      : Amazonka.APIGateway.ImportApiKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import API keys from an external source, such as a CSV-formatted file.
module Amazonka.APIGateway.ImportApiKeys
  ( -- * Creating a Request
    ImportApiKeys (..),
    newImportApiKeys,

    -- * Request Lenses
    importApiKeys_failOnWarnings,
    importApiKeys_body,
    importApiKeys_format,

    -- * Destructuring the Response
    ImportApiKeysResponse (..),
    newImportApiKeysResponse,

    -- * Response Lenses
    importApiKeysResponse_ids,
    importApiKeysResponse_warnings,
    importApiKeysResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The POST request to import API keys from an external source, such as a
-- CSV-formatted file.
--
-- /See:/ 'newImportApiKeys' smart constructor.
data ImportApiKeys = ImportApiKeys'
  { -- | A query parameter to indicate whether to rollback ApiKey importation
    -- (@true@) or not (@false@) when error is encountered.
    failOnWarnings :: Prelude.Maybe Prelude.Bool,
    -- | The payload of the POST request to import API keys. For the payload
    -- format, see API Key File Format.
    body :: Prelude.ByteString,
    -- | A query parameter to specify the input format to imported API keys.
    -- Currently, only the @csv@ format is supported.
    format :: ApiKeysFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportApiKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failOnWarnings', 'importApiKeys_failOnWarnings' - A query parameter to indicate whether to rollback ApiKey importation
-- (@true@) or not (@false@) when error is encountered.
--
-- 'body', 'importApiKeys_body' - The payload of the POST request to import API keys. For the payload
-- format, see API Key File Format.
--
-- 'format', 'importApiKeys_format' - A query parameter to specify the input format to imported API keys.
-- Currently, only the @csv@ format is supported.
newImportApiKeys ::
  -- | 'body'
  Prelude.ByteString ->
  -- | 'format'
  ApiKeysFormat ->
  ImportApiKeys
newImportApiKeys pBody_ pFormat_ =
  ImportApiKeys'
    { failOnWarnings = Prelude.Nothing,
      body = pBody_,
      format = pFormat_
    }

-- | A query parameter to indicate whether to rollback ApiKey importation
-- (@true@) or not (@false@) when error is encountered.
importApiKeys_failOnWarnings :: Lens.Lens' ImportApiKeys (Prelude.Maybe Prelude.Bool)
importApiKeys_failOnWarnings = Lens.lens (\ImportApiKeys' {failOnWarnings} -> failOnWarnings) (\s@ImportApiKeys' {} a -> s {failOnWarnings = a} :: ImportApiKeys)

-- | The payload of the POST request to import API keys. For the payload
-- format, see API Key File Format.
importApiKeys_body :: Lens.Lens' ImportApiKeys Prelude.ByteString
importApiKeys_body = Lens.lens (\ImportApiKeys' {body} -> body) (\s@ImportApiKeys' {} a -> s {body = a} :: ImportApiKeys)

-- | A query parameter to specify the input format to imported API keys.
-- Currently, only the @csv@ format is supported.
importApiKeys_format :: Lens.Lens' ImportApiKeys ApiKeysFormat
importApiKeys_format = Lens.lens (\ImportApiKeys' {format} -> format) (\s@ImportApiKeys' {} a -> s {format = a} :: ImportApiKeys)

instance Core.AWSRequest ImportApiKeys where
  type
    AWSResponse ImportApiKeys =
      ImportApiKeysResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportApiKeysResponse'
            Prelude.<$> (x Data..?> "ids" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportApiKeys where
  hashWithSalt _salt ImportApiKeys' {..} =
    _salt
      `Prelude.hashWithSalt` failOnWarnings
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` format

instance Prelude.NFData ImportApiKeys where
  rnf ImportApiKeys' {..} =
    Prelude.rnf failOnWarnings `Prelude.seq`
      Prelude.rnf body `Prelude.seq`
        Prelude.rnf format

instance Data.ToBody ImportApiKeys where
  toBody ImportApiKeys' {..} = Data.toBody body

instance Data.ToHeaders ImportApiKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath ImportApiKeys where
  toPath = Prelude.const "/apikeys"

instance Data.ToQuery ImportApiKeys where
  toQuery ImportApiKeys' {..} =
    Prelude.mconcat
      [ "failonwarnings" Data.=: failOnWarnings,
        "format" Data.=: format,
        "mode=import"
      ]

-- | The identifier of an ApiKey used in a UsagePlan.
--
-- /See:/ 'newImportApiKeysResponse' smart constructor.
data ImportApiKeysResponse = ImportApiKeysResponse'
  { -- | A list of all the ApiKey identifiers.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | A list of warning messages.
    warnings :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportApiKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'importApiKeysResponse_ids' - A list of all the ApiKey identifiers.
--
-- 'warnings', 'importApiKeysResponse_warnings' - A list of warning messages.
--
-- 'httpStatus', 'importApiKeysResponse_httpStatus' - The response's http status code.
newImportApiKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportApiKeysResponse
newImportApiKeysResponse pHttpStatus_ =
  ImportApiKeysResponse'
    { ids = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all the ApiKey identifiers.
importApiKeysResponse_ids :: Lens.Lens' ImportApiKeysResponse (Prelude.Maybe [Prelude.Text])
importApiKeysResponse_ids = Lens.lens (\ImportApiKeysResponse' {ids} -> ids) (\s@ImportApiKeysResponse' {} a -> s {ids = a} :: ImportApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of warning messages.
importApiKeysResponse_warnings :: Lens.Lens' ImportApiKeysResponse (Prelude.Maybe [Prelude.Text])
importApiKeysResponse_warnings = Lens.lens (\ImportApiKeysResponse' {warnings} -> warnings) (\s@ImportApiKeysResponse' {} a -> s {warnings = a} :: ImportApiKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
importApiKeysResponse_httpStatus :: Lens.Lens' ImportApiKeysResponse Prelude.Int
importApiKeysResponse_httpStatus = Lens.lens (\ImportApiKeysResponse' {httpStatus} -> httpStatus) (\s@ImportApiKeysResponse' {} a -> s {httpStatus = a} :: ImportApiKeysResponse)

instance Prelude.NFData ImportApiKeysResponse where
  rnf ImportApiKeysResponse' {..} =
    Prelude.rnf ids `Prelude.seq`
      Prelude.rnf warnings `Prelude.seq`
        Prelude.rnf httpStatus
