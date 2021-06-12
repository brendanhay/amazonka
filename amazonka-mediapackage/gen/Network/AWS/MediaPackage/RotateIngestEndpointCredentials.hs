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
-- Module      : Network.AWS.MediaPackage.RotateIngestEndpointCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotate the IngestEndpoint\'s username and password, as specified by the
-- IngestEndpoint\'s id.
module Network.AWS.MediaPackage.RotateIngestEndpointCredentials
  ( -- * Creating a Request
    RotateIngestEndpointCredentials (..),
    newRotateIngestEndpointCredentials,

    -- * Request Lenses
    rotateIngestEndpointCredentials_ingestEndpointId,
    rotateIngestEndpointCredentials_id,

    -- * Destructuring the Response
    RotateIngestEndpointCredentialsResponse (..),
    newRotateIngestEndpointCredentialsResponse,

    -- * Response Lenses
    rotateIngestEndpointCredentialsResponse_egressAccessLogs,
    rotateIngestEndpointCredentialsResponse_hlsIngest,
    rotateIngestEndpointCredentialsResponse_arn,
    rotateIngestEndpointCredentialsResponse_id,
    rotateIngestEndpointCredentialsResponse_ingressAccessLogs,
    rotateIngestEndpointCredentialsResponse_tags,
    rotateIngestEndpointCredentialsResponse_description,
    rotateIngestEndpointCredentialsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRotateIngestEndpointCredentials' smart constructor.
data RotateIngestEndpointCredentials = RotateIngestEndpointCredentials'
  { -- | The id of the IngestEndpoint whose credentials should be rotated
    ingestEndpointId :: Core.Text,
    -- | The ID of the channel the IngestEndpoint is on.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RotateIngestEndpointCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestEndpointId', 'rotateIngestEndpointCredentials_ingestEndpointId' - The id of the IngestEndpoint whose credentials should be rotated
--
-- 'id', 'rotateIngestEndpointCredentials_id' - The ID of the channel the IngestEndpoint is on.
newRotateIngestEndpointCredentials ::
  -- | 'ingestEndpointId'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  RotateIngestEndpointCredentials
newRotateIngestEndpointCredentials
  pIngestEndpointId_
  pId_ =
    RotateIngestEndpointCredentials'
      { ingestEndpointId =
          pIngestEndpointId_,
        id = pId_
      }

-- | The id of the IngestEndpoint whose credentials should be rotated
rotateIngestEndpointCredentials_ingestEndpointId :: Lens.Lens' RotateIngestEndpointCredentials Core.Text
rotateIngestEndpointCredentials_ingestEndpointId = Lens.lens (\RotateIngestEndpointCredentials' {ingestEndpointId} -> ingestEndpointId) (\s@RotateIngestEndpointCredentials' {} a -> s {ingestEndpointId = a} :: RotateIngestEndpointCredentials)

-- | The ID of the channel the IngestEndpoint is on.
rotateIngestEndpointCredentials_id :: Lens.Lens' RotateIngestEndpointCredentials Core.Text
rotateIngestEndpointCredentials_id = Lens.lens (\RotateIngestEndpointCredentials' {id} -> id) (\s@RotateIngestEndpointCredentials' {} a -> s {id = a} :: RotateIngestEndpointCredentials)

instance
  Core.AWSRequest
    RotateIngestEndpointCredentials
  where
  type
    AWSResponse RotateIngestEndpointCredentials =
      RotateIngestEndpointCredentialsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RotateIngestEndpointCredentialsResponse'
            Core.<$> (x Core..?> "egressAccessLogs")
            Core.<*> (x Core..?> "hlsIngest")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "ingressAccessLogs")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RotateIngestEndpointCredentials

instance Core.NFData RotateIngestEndpointCredentials

instance
  Core.ToHeaders
    RotateIngestEndpointCredentials
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RotateIngestEndpointCredentials where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath RotateIngestEndpointCredentials where
  toPath RotateIngestEndpointCredentials' {..} =
    Core.mconcat
      [ "/channels/",
        Core.toBS id,
        "/ingest_endpoints/",
        Core.toBS ingestEndpointId,
        "/credentials"
      ]

instance Core.ToQuery RotateIngestEndpointCredentials where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRotateIngestEndpointCredentialsResponse' smart constructor.
data RotateIngestEndpointCredentialsResponse = RotateIngestEndpointCredentialsResponse'
  { egressAccessLogs :: Core.Maybe EgressAccessLogs,
    hlsIngest :: Core.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the Channel.
    id :: Core.Maybe Core.Text,
    ingressAccessLogs :: Core.Maybe IngressAccessLogs,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RotateIngestEndpointCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'rotateIngestEndpointCredentialsResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'rotateIngestEndpointCredentialsResponse_hlsIngest' - Undocumented member.
--
-- 'arn', 'rotateIngestEndpointCredentialsResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'rotateIngestEndpointCredentialsResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'rotateIngestEndpointCredentialsResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'rotateIngestEndpointCredentialsResponse_tags' - Undocumented member.
--
-- 'description', 'rotateIngestEndpointCredentialsResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'rotateIngestEndpointCredentialsResponse_httpStatus' - The response's http status code.
newRotateIngestEndpointCredentialsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RotateIngestEndpointCredentialsResponse
newRotateIngestEndpointCredentialsResponse
  pHttpStatus_ =
    RotateIngestEndpointCredentialsResponse'
      { egressAccessLogs =
          Core.Nothing,
        hlsIngest = Core.Nothing,
        arn = Core.Nothing,
        id = Core.Nothing,
        ingressAccessLogs = Core.Nothing,
        tags = Core.Nothing,
        description = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_egressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe EgressAccessLogs)
rotateIngestEndpointCredentialsResponse_egressAccessLogs = Lens.lens (\RotateIngestEndpointCredentialsResponse' {egressAccessLogs} -> egressAccessLogs) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {egressAccessLogs = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_hlsIngest :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe HlsIngest)
rotateIngestEndpointCredentialsResponse_hlsIngest = Lens.lens (\RotateIngestEndpointCredentialsResponse' {hlsIngest} -> hlsIngest) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {hlsIngest = a} :: RotateIngestEndpointCredentialsResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
rotateIngestEndpointCredentialsResponse_arn :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Core.Text)
rotateIngestEndpointCredentialsResponse_arn = Lens.lens (\RotateIngestEndpointCredentialsResponse' {arn} -> arn) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {arn = a} :: RotateIngestEndpointCredentialsResponse)

-- | The ID of the Channel.
rotateIngestEndpointCredentialsResponse_id :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Core.Text)
rotateIngestEndpointCredentialsResponse_id = Lens.lens (\RotateIngestEndpointCredentialsResponse' {id} -> id) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {id = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_ingressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe IngressAccessLogs)
rotateIngestEndpointCredentialsResponse_ingressAccessLogs = Lens.lens (\RotateIngestEndpointCredentialsResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {ingressAccessLogs = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_tags :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
rotateIngestEndpointCredentialsResponse_tags = Lens.lens (\RotateIngestEndpointCredentialsResponse' {tags} -> tags) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {tags = a} :: RotateIngestEndpointCredentialsResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
rotateIngestEndpointCredentialsResponse_description :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Core.Maybe Core.Text)
rotateIngestEndpointCredentialsResponse_description = Lens.lens (\RotateIngestEndpointCredentialsResponse' {description} -> description) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {description = a} :: RotateIngestEndpointCredentialsResponse)

-- | The response's http status code.
rotateIngestEndpointCredentialsResponse_httpStatus :: Lens.Lens' RotateIngestEndpointCredentialsResponse Core.Int
rotateIngestEndpointCredentialsResponse_httpStatus = Lens.lens (\RotateIngestEndpointCredentialsResponse' {httpStatus} -> httpStatus) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {httpStatus = a} :: RotateIngestEndpointCredentialsResponse)

instance
  Core.NFData
    RotateIngestEndpointCredentialsResponse
