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
-- Module      : Amazonka.SSM.GetOpsMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View operational metadata related to an application in Application
-- Manager.
module Amazonka.SSM.GetOpsMetadata
  ( -- * Creating a Request
    GetOpsMetadata (..),
    newGetOpsMetadata,

    -- * Request Lenses
    getOpsMetadata_maxResults,
    getOpsMetadata_nextToken,
    getOpsMetadata_opsMetadataArn,

    -- * Destructuring the Response
    GetOpsMetadataResponse (..),
    newGetOpsMetadataResponse,

    -- * Response Lenses
    getOpsMetadataResponse_metadata,
    getOpsMetadataResponse_nextToken,
    getOpsMetadataResponse_resourceId,
    getOpsMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetOpsMetadata' smart constructor.
data GetOpsMetadata = GetOpsMetadata'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an OpsMetadata Object to view.
    opsMetadataArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getOpsMetadata_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'getOpsMetadata_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'opsMetadataArn', 'getOpsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of an OpsMetadata Object to view.
newGetOpsMetadata ::
  -- | 'opsMetadataArn'
  Prelude.Text ->
  GetOpsMetadata
newGetOpsMetadata pOpsMetadataArn_ =
  GetOpsMetadata'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      opsMetadataArn = pOpsMetadataArn_
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getOpsMetadata_maxResults :: Lens.Lens' GetOpsMetadata (Prelude.Maybe Prelude.Natural)
getOpsMetadata_maxResults = Lens.lens (\GetOpsMetadata' {maxResults} -> maxResults) (\s@GetOpsMetadata' {} a -> s {maxResults = a} :: GetOpsMetadata)

-- | A token to start the list. Use this token to get the next set of
-- results.
getOpsMetadata_nextToken :: Lens.Lens' GetOpsMetadata (Prelude.Maybe Prelude.Text)
getOpsMetadata_nextToken = Lens.lens (\GetOpsMetadata' {nextToken} -> nextToken) (\s@GetOpsMetadata' {} a -> s {nextToken = a} :: GetOpsMetadata)

-- | The Amazon Resource Name (ARN) of an OpsMetadata Object to view.
getOpsMetadata_opsMetadataArn :: Lens.Lens' GetOpsMetadata Prelude.Text
getOpsMetadata_opsMetadataArn = Lens.lens (\GetOpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@GetOpsMetadata' {} a -> s {opsMetadataArn = a} :: GetOpsMetadata)

instance Core.AWSRequest GetOpsMetadata where
  type
    AWSResponse GetOpsMetadata =
      GetOpsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpsMetadataResponse'
            Prelude.<$> (x Data..?> "Metadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOpsMetadata where
  hashWithSalt _salt GetOpsMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` opsMetadataArn

instance Prelude.NFData GetOpsMetadata where
  rnf GetOpsMetadata' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf opsMetadataArn

instance Data.ToHeaders GetOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.GetOpsMetadata" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOpsMetadata where
  toJSON GetOpsMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OpsMetadataArn" Data..= opsMetadataArn)
          ]
      )

instance Data.ToPath GetOpsMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOpsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOpsMetadataResponse' smart constructor.
data GetOpsMetadataResponse = GetOpsMetadataResponse'
  { -- | OpsMetadata for an Application Manager application.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue),
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the Application Manager application.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'getOpsMetadataResponse_metadata' - OpsMetadata for an Application Manager application.
--
-- 'nextToken', 'getOpsMetadataResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'resourceId', 'getOpsMetadataResponse_resourceId' - The resource ID of the Application Manager application.
--
-- 'httpStatus', 'getOpsMetadataResponse_httpStatus' - The response's http status code.
newGetOpsMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOpsMetadataResponse
newGetOpsMetadataResponse pHttpStatus_ =
  GetOpsMetadataResponse'
    { metadata = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | OpsMetadata for an Application Manager application.
getOpsMetadataResponse_metadata :: Lens.Lens' GetOpsMetadataResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue))
getOpsMetadataResponse_metadata = Lens.lens (\GetOpsMetadataResponse' {metadata} -> metadata) (\s@GetOpsMetadataResponse' {} a -> s {metadata = a} :: GetOpsMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
getOpsMetadataResponse_nextToken :: Lens.Lens' GetOpsMetadataResponse (Prelude.Maybe Prelude.Text)
getOpsMetadataResponse_nextToken = Lens.lens (\GetOpsMetadataResponse' {nextToken} -> nextToken) (\s@GetOpsMetadataResponse' {} a -> s {nextToken = a} :: GetOpsMetadataResponse)

-- | The resource ID of the Application Manager application.
getOpsMetadataResponse_resourceId :: Lens.Lens' GetOpsMetadataResponse (Prelude.Maybe Prelude.Text)
getOpsMetadataResponse_resourceId = Lens.lens (\GetOpsMetadataResponse' {resourceId} -> resourceId) (\s@GetOpsMetadataResponse' {} a -> s {resourceId = a} :: GetOpsMetadataResponse)

-- | The response's http status code.
getOpsMetadataResponse_httpStatus :: Lens.Lens' GetOpsMetadataResponse Prelude.Int
getOpsMetadataResponse_httpStatus = Lens.lens (\GetOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@GetOpsMetadataResponse' {} a -> s {httpStatus = a} :: GetOpsMetadataResponse)

instance Prelude.NFData GetOpsMetadataResponse where
  rnf GetOpsMetadataResponse' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpStatus
