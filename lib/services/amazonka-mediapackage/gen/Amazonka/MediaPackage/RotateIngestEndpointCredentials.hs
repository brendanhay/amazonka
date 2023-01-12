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
-- Module      : Amazonka.MediaPackage.RotateIngestEndpointCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotate the IngestEndpoint\'s username and password, as specified by the
-- IngestEndpoint\'s id.
module Amazonka.MediaPackage.RotateIngestEndpointCredentials
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
    rotateIngestEndpointCredentialsResponse_arn,
    rotateIngestEndpointCredentialsResponse_description,
    rotateIngestEndpointCredentialsResponse_egressAccessLogs,
    rotateIngestEndpointCredentialsResponse_hlsIngest,
    rotateIngestEndpointCredentialsResponse_id,
    rotateIngestEndpointCredentialsResponse_ingressAccessLogs,
    rotateIngestEndpointCredentialsResponse_tags,
    rotateIngestEndpointCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRotateIngestEndpointCredentials' smart constructor.
data RotateIngestEndpointCredentials = RotateIngestEndpointCredentials'
  { -- | The id of the IngestEndpoint whose credentials should be rotated
    ingestEndpointId :: Prelude.Text,
    -- | The ID of the channel the IngestEndpoint is on.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
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
rotateIngestEndpointCredentials_ingestEndpointId :: Lens.Lens' RotateIngestEndpointCredentials Prelude.Text
rotateIngestEndpointCredentials_ingestEndpointId = Lens.lens (\RotateIngestEndpointCredentials' {ingestEndpointId} -> ingestEndpointId) (\s@RotateIngestEndpointCredentials' {} a -> s {ingestEndpointId = a} :: RotateIngestEndpointCredentials)

-- | The ID of the channel the IngestEndpoint is on.
rotateIngestEndpointCredentials_id :: Lens.Lens' RotateIngestEndpointCredentials Prelude.Text
rotateIngestEndpointCredentials_id = Lens.lens (\RotateIngestEndpointCredentials' {id} -> id) (\s@RotateIngestEndpointCredentials' {} a -> s {id = a} :: RotateIngestEndpointCredentials)

instance
  Core.AWSRequest
    RotateIngestEndpointCredentials
  where
  type
    AWSResponse RotateIngestEndpointCredentials =
      RotateIngestEndpointCredentialsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RotateIngestEndpointCredentialsResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "hlsIngest")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "ingressAccessLogs")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RotateIngestEndpointCredentials
  where
  hashWithSalt
    _salt
    RotateIngestEndpointCredentials' {..} =
      _salt `Prelude.hashWithSalt` ingestEndpointId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    RotateIngestEndpointCredentials
  where
  rnf RotateIngestEndpointCredentials' {..} =
    Prelude.rnf ingestEndpointId
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    RotateIngestEndpointCredentials
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RotateIngestEndpointCredentials where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RotateIngestEndpointCredentials where
  toPath RotateIngestEndpointCredentials' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS id,
        "/ingest_endpoints/",
        Data.toBS ingestEndpointId,
        "/credentials"
      ]

instance Data.ToQuery RotateIngestEndpointCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRotateIngestEndpointCredentialsResponse' smart constructor.
data RotateIngestEndpointCredentialsResponse = RotateIngestEndpointCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotateIngestEndpointCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'rotateIngestEndpointCredentialsResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'description', 'rotateIngestEndpointCredentialsResponse_description' - A short text description of the Channel.
--
-- 'egressAccessLogs', 'rotateIngestEndpointCredentialsResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'rotateIngestEndpointCredentialsResponse_hlsIngest' - Undocumented member.
--
-- 'id', 'rotateIngestEndpointCredentialsResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'rotateIngestEndpointCredentialsResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'rotateIngestEndpointCredentialsResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'rotateIngestEndpointCredentialsResponse_httpStatus' - The response's http status code.
newRotateIngestEndpointCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RotateIngestEndpointCredentialsResponse
newRotateIngestEndpointCredentialsResponse
  pHttpStatus_ =
    RotateIngestEndpointCredentialsResponse'
      { arn =
          Prelude.Nothing,
        description = Prelude.Nothing,
        egressAccessLogs = Prelude.Nothing,
        hlsIngest = Prelude.Nothing,
        id = Prelude.Nothing,
        ingressAccessLogs =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) assigned to the Channel.
rotateIngestEndpointCredentialsResponse_arn :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe Prelude.Text)
rotateIngestEndpointCredentialsResponse_arn = Lens.lens (\RotateIngestEndpointCredentialsResponse' {arn} -> arn) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {arn = a} :: RotateIngestEndpointCredentialsResponse)

-- | A short text description of the Channel.
rotateIngestEndpointCredentialsResponse_description :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe Prelude.Text)
rotateIngestEndpointCredentialsResponse_description = Lens.lens (\RotateIngestEndpointCredentialsResponse' {description} -> description) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {description = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_egressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe EgressAccessLogs)
rotateIngestEndpointCredentialsResponse_egressAccessLogs = Lens.lens (\RotateIngestEndpointCredentialsResponse' {egressAccessLogs} -> egressAccessLogs) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {egressAccessLogs = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_hlsIngest :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe HlsIngest)
rotateIngestEndpointCredentialsResponse_hlsIngest = Lens.lens (\RotateIngestEndpointCredentialsResponse' {hlsIngest} -> hlsIngest) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {hlsIngest = a} :: RotateIngestEndpointCredentialsResponse)

-- | The ID of the Channel.
rotateIngestEndpointCredentialsResponse_id :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe Prelude.Text)
rotateIngestEndpointCredentialsResponse_id = Lens.lens (\RotateIngestEndpointCredentialsResponse' {id} -> id) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {id = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_ingressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe IngressAccessLogs)
rotateIngestEndpointCredentialsResponse_ingressAccessLogs = Lens.lens (\RotateIngestEndpointCredentialsResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {ingressAccessLogs = a} :: RotateIngestEndpointCredentialsResponse)

-- | Undocumented member.
rotateIngestEndpointCredentialsResponse_tags :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rotateIngestEndpointCredentialsResponse_tags = Lens.lens (\RotateIngestEndpointCredentialsResponse' {tags} -> tags) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {tags = a} :: RotateIngestEndpointCredentialsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
rotateIngestEndpointCredentialsResponse_httpStatus :: Lens.Lens' RotateIngestEndpointCredentialsResponse Prelude.Int
rotateIngestEndpointCredentialsResponse_httpStatus = Lens.lens (\RotateIngestEndpointCredentialsResponse' {httpStatus} -> httpStatus) (\s@RotateIngestEndpointCredentialsResponse' {} a -> s {httpStatus = a} :: RotateIngestEndpointCredentialsResponse)

instance
  Prelude.NFData
    RotateIngestEndpointCredentialsResponse
  where
  rnf RotateIngestEndpointCredentialsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf hlsIngest
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ingressAccessLogs
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
