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
-- Module      : Network.AWS.SSM.GetOpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View operational metadata related to an application in Application
-- Manager.
module Network.AWS.SSM.GetOpsMetadata
  ( -- * Creating a Request
    GetOpsMetadata (..),
    newGetOpsMetadata,

    -- * Request Lenses
    getOpsMetadata_nextToken,
    getOpsMetadata_maxResults,
    getOpsMetadata_opsMetadataArn,

    -- * Destructuring the Response
    GetOpsMetadataResponse (..),
    newGetOpsMetadataResponse,

    -- * Response Lenses
    getOpsMetadataResponse_resourceId,
    getOpsMetadataResponse_nextToken,
    getOpsMetadataResponse_metadata,
    getOpsMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetOpsMetadata' smart constructor.
data GetOpsMetadata = GetOpsMetadata'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of an OpsMetadata Object to view.
    opsMetadataArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOpsMetadata_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'getOpsMetadata_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'opsMetadataArn', 'getOpsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of an OpsMetadata Object to view.
newGetOpsMetadata ::
  -- | 'opsMetadataArn'
  Core.Text ->
  GetOpsMetadata
newGetOpsMetadata pOpsMetadataArn_ =
  GetOpsMetadata'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      opsMetadataArn = pOpsMetadataArn_
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
getOpsMetadata_nextToken :: Lens.Lens' GetOpsMetadata (Core.Maybe Core.Text)
getOpsMetadata_nextToken = Lens.lens (\GetOpsMetadata' {nextToken} -> nextToken) (\s@GetOpsMetadata' {} a -> s {nextToken = a} :: GetOpsMetadata)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getOpsMetadata_maxResults :: Lens.Lens' GetOpsMetadata (Core.Maybe Core.Natural)
getOpsMetadata_maxResults = Lens.lens (\GetOpsMetadata' {maxResults} -> maxResults) (\s@GetOpsMetadata' {} a -> s {maxResults = a} :: GetOpsMetadata)

-- | The Amazon Resource Name (ARN) of an OpsMetadata Object to view.
getOpsMetadata_opsMetadataArn :: Lens.Lens' GetOpsMetadata Core.Text
getOpsMetadata_opsMetadataArn = Lens.lens (\GetOpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@GetOpsMetadata' {} a -> s {opsMetadataArn = a} :: GetOpsMetadata)

instance Core.AWSRequest GetOpsMetadata where
  type
    AWSResponse GetOpsMetadata =
      GetOpsMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpsMetadataResponse'
            Core.<$> (x Core..?> "ResourceId")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Metadata" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOpsMetadata

instance Core.NFData GetOpsMetadata

instance Core.ToHeaders GetOpsMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetOpsMetadata" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetOpsMetadata where
  toJSON GetOpsMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OpsMetadataArn" Core..= opsMetadataArn)
          ]
      )

instance Core.ToPath GetOpsMetadata where
  toPath = Core.const "/"

instance Core.ToQuery GetOpsMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOpsMetadataResponse' smart constructor.
data GetOpsMetadataResponse = GetOpsMetadataResponse'
  { -- | The resource ID of the Application Manager application.
    resourceId :: Core.Maybe Core.Text,
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | OpsMetadata for an Application Manager application.
    metadata :: Core.Maybe (Core.HashMap Core.Text MetadataValue),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'getOpsMetadataResponse_resourceId' - The resource ID of the Application Manager application.
--
-- 'nextToken', 'getOpsMetadataResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'metadata', 'getOpsMetadataResponse_metadata' - OpsMetadata for an Application Manager application.
--
-- 'httpStatus', 'getOpsMetadataResponse_httpStatus' - The response's http status code.
newGetOpsMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOpsMetadataResponse
newGetOpsMetadataResponse pHttpStatus_ =
  GetOpsMetadataResponse'
    { resourceId = Core.Nothing,
      nextToken = Core.Nothing,
      metadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource ID of the Application Manager application.
getOpsMetadataResponse_resourceId :: Lens.Lens' GetOpsMetadataResponse (Core.Maybe Core.Text)
getOpsMetadataResponse_resourceId = Lens.lens (\GetOpsMetadataResponse' {resourceId} -> resourceId) (\s@GetOpsMetadataResponse' {} a -> s {resourceId = a} :: GetOpsMetadataResponse)

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
getOpsMetadataResponse_nextToken :: Lens.Lens' GetOpsMetadataResponse (Core.Maybe Core.Text)
getOpsMetadataResponse_nextToken = Lens.lens (\GetOpsMetadataResponse' {nextToken} -> nextToken) (\s@GetOpsMetadataResponse' {} a -> s {nextToken = a} :: GetOpsMetadataResponse)

-- | OpsMetadata for an Application Manager application.
getOpsMetadataResponse_metadata :: Lens.Lens' GetOpsMetadataResponse (Core.Maybe (Core.HashMap Core.Text MetadataValue))
getOpsMetadataResponse_metadata = Lens.lens (\GetOpsMetadataResponse' {metadata} -> metadata) (\s@GetOpsMetadataResponse' {} a -> s {metadata = a} :: GetOpsMetadataResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getOpsMetadataResponse_httpStatus :: Lens.Lens' GetOpsMetadataResponse Core.Int
getOpsMetadataResponse_httpStatus = Lens.lens (\GetOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@GetOpsMetadataResponse' {} a -> s {httpStatus = a} :: GetOpsMetadataResponse)

instance Core.NFData GetOpsMetadataResponse
