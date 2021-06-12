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
-- Module      : Network.AWS.APIGateway.ImportApiKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import API keys from an external source, such as a CSV-formatted file.
module Network.AWS.APIGateway.ImportApiKeys
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
    importApiKeysResponse_warnings,
    importApiKeysResponse_ids,
    importApiKeysResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST request to import API keys from an external source, such as a
-- CSV-formatted file.
--
-- /See:/ 'newImportApiKeys' smart constructor.
data ImportApiKeys = ImportApiKeys'
  { -- | A query parameter to indicate whether to rollback ApiKey importation
    -- (@true@) or not (@false@) when error is encountered.
    failOnWarnings :: Core.Maybe Core.Bool,
    -- | The payload of the POST request to import API keys. For the payload
    -- format, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format>.
    body :: Core.ByteString,
    -- | A query parameter to specify the input format to imported API keys.
    -- Currently, only the @csv@ format is supported.
    format :: ApiKeysFormat
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- format, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format>.
--
-- 'format', 'importApiKeys_format' - A query parameter to specify the input format to imported API keys.
-- Currently, only the @csv@ format is supported.
newImportApiKeys ::
  -- | 'body'
  Core.ByteString ->
  -- | 'format'
  ApiKeysFormat ->
  ImportApiKeys
newImportApiKeys pBody_ pFormat_ =
  ImportApiKeys'
    { failOnWarnings = Core.Nothing,
      body = pBody_,
      format = pFormat_
    }

-- | A query parameter to indicate whether to rollback ApiKey importation
-- (@true@) or not (@false@) when error is encountered.
importApiKeys_failOnWarnings :: Lens.Lens' ImportApiKeys (Core.Maybe Core.Bool)
importApiKeys_failOnWarnings = Lens.lens (\ImportApiKeys' {failOnWarnings} -> failOnWarnings) (\s@ImportApiKeys' {} a -> s {failOnWarnings = a} :: ImportApiKeys)

-- | The payload of the POST request to import API keys. For the payload
-- format, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format>.
importApiKeys_body :: Lens.Lens' ImportApiKeys Core.ByteString
importApiKeys_body = Lens.lens (\ImportApiKeys' {body} -> body) (\s@ImportApiKeys' {} a -> s {body = a} :: ImportApiKeys)

-- | A query parameter to specify the input format to imported API keys.
-- Currently, only the @csv@ format is supported.
importApiKeys_format :: Lens.Lens' ImportApiKeys ApiKeysFormat
importApiKeys_format = Lens.lens (\ImportApiKeys' {format} -> format) (\s@ImportApiKeys' {} a -> s {format = a} :: ImportApiKeys)

instance Core.AWSRequest ImportApiKeys where
  type
    AWSResponse ImportApiKeys =
      ImportApiKeysResponse
  request = Request.postBody defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportApiKeysResponse'
            Core.<$> (x Core..?> "warnings" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ids" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportApiKeys

instance Core.NFData ImportApiKeys

instance Core.ToBody ImportApiKeys where
  toBody ImportApiKeys' {..} = Core.toBody body

instance Core.ToHeaders ImportApiKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath ImportApiKeys where
  toPath = Core.const "/apikeys"

instance Core.ToQuery ImportApiKeys where
  toQuery ImportApiKeys' {..} =
    Core.mconcat
      [ "failonwarnings" Core.=: failOnWarnings,
        "format" Core.=: format,
        "mode=import"
      ]

-- | The identifier of an ApiKey used in a UsagePlan.
--
-- /See:/ 'newImportApiKeysResponse' smart constructor.
data ImportApiKeysResponse = ImportApiKeysResponse'
  { -- | A list of warning messages.
    warnings :: Core.Maybe [Core.Text],
    -- | A list of all the ApiKey identifiers.
    ids :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportApiKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warnings', 'importApiKeysResponse_warnings' - A list of warning messages.
--
-- 'ids', 'importApiKeysResponse_ids' - A list of all the ApiKey identifiers.
--
-- 'httpStatus', 'importApiKeysResponse_httpStatus' - The response's http status code.
newImportApiKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ImportApiKeysResponse
newImportApiKeysResponse pHttpStatus_ =
  ImportApiKeysResponse'
    { warnings = Core.Nothing,
      ids = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of warning messages.
importApiKeysResponse_warnings :: Lens.Lens' ImportApiKeysResponse (Core.Maybe [Core.Text])
importApiKeysResponse_warnings = Lens.lens (\ImportApiKeysResponse' {warnings} -> warnings) (\s@ImportApiKeysResponse' {} a -> s {warnings = a} :: ImportApiKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of all the ApiKey identifiers.
importApiKeysResponse_ids :: Lens.Lens' ImportApiKeysResponse (Core.Maybe [Core.Text])
importApiKeysResponse_ids = Lens.lens (\ImportApiKeysResponse' {ids} -> ids) (\s@ImportApiKeysResponse' {} a -> s {ids = a} :: ImportApiKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
importApiKeysResponse_httpStatus :: Lens.Lens' ImportApiKeysResponse Core.Int
importApiKeysResponse_httpStatus = Lens.lens (\ImportApiKeysResponse' {httpStatus} -> httpStatus) (\s@ImportApiKeysResponse' {} a -> s {httpStatus = a} :: ImportApiKeysResponse)

instance Core.NFData ImportApiKeysResponse
