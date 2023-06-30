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
-- Module      : Amazonka.MediaConnect.AddFlowMediaStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds media streams to an existing flow. After you add a media stream to
-- a flow, you can associate it with a source and\/or an output that uses
-- the ST 2110 JPEG XS or CDI protocol.
module Amazonka.MediaConnect.AddFlowMediaStreams
  ( -- * Creating a Request
    AddFlowMediaStreams (..),
    newAddFlowMediaStreams,

    -- * Request Lenses
    addFlowMediaStreams_flowArn,
    addFlowMediaStreams_mediaStreams,

    -- * Destructuring the Response
    AddFlowMediaStreamsResponse (..),
    newAddFlowMediaStreamsResponse,

    -- * Response Lenses
    addFlowMediaStreamsResponse_flowArn,
    addFlowMediaStreamsResponse_mediaStreams,
    addFlowMediaStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to add media streams to the flow.
--
-- /See:/ 'newAddFlowMediaStreams' smart constructor.
data AddFlowMediaStreams = AddFlowMediaStreams'
  { -- | The Amazon Resource Name (ARN) of the flow.
    flowArn :: Prelude.Text,
    -- | The media streams that you want to add to the flow.
    mediaStreams :: [AddMediaStreamRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowMediaStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowMediaStreams_flowArn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'mediaStreams', 'addFlowMediaStreams_mediaStreams' - The media streams that you want to add to the flow.
newAddFlowMediaStreams ::
  -- | 'flowArn'
  Prelude.Text ->
  AddFlowMediaStreams
newAddFlowMediaStreams pFlowArn_ =
  AddFlowMediaStreams'
    { flowArn = pFlowArn_,
      mediaStreams = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the flow.
addFlowMediaStreams_flowArn :: Lens.Lens' AddFlowMediaStreams Prelude.Text
addFlowMediaStreams_flowArn = Lens.lens (\AddFlowMediaStreams' {flowArn} -> flowArn) (\s@AddFlowMediaStreams' {} a -> s {flowArn = a} :: AddFlowMediaStreams)

-- | The media streams that you want to add to the flow.
addFlowMediaStreams_mediaStreams :: Lens.Lens' AddFlowMediaStreams [AddMediaStreamRequest]
addFlowMediaStreams_mediaStreams = Lens.lens (\AddFlowMediaStreams' {mediaStreams} -> mediaStreams) (\s@AddFlowMediaStreams' {} a -> s {mediaStreams = a} :: AddFlowMediaStreams) Prelude.. Lens.coerced

instance Core.AWSRequest AddFlowMediaStreams where
  type
    AWSResponse AddFlowMediaStreams =
      AddFlowMediaStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddFlowMediaStreamsResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "mediaStreams" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddFlowMediaStreams where
  hashWithSalt _salt AddFlowMediaStreams' {..} =
    _salt
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` mediaStreams

instance Prelude.NFData AddFlowMediaStreams where
  rnf AddFlowMediaStreams' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf mediaStreams

instance Data.ToHeaders AddFlowMediaStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddFlowMediaStreams where
  toJSON AddFlowMediaStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("mediaStreams" Data..= mediaStreams)]
      )

instance Data.ToPath AddFlowMediaStreams where
  toPath AddFlowMediaStreams' {..} =
    Prelude.mconcat
      ["/v1/flows/", Data.toBS flowArn, "/mediaStreams"]

instance Data.ToQuery AddFlowMediaStreams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddFlowMediaStreamsResponse' smart constructor.
data AddFlowMediaStreamsResponse = AddFlowMediaStreamsResponse'
  { -- | The ARN of the flow that you added media streams to.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The media streams that you added to the flow.
    mediaStreams :: Prelude.Maybe [MediaStream],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddFlowMediaStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'addFlowMediaStreamsResponse_flowArn' - The ARN of the flow that you added media streams to.
--
-- 'mediaStreams', 'addFlowMediaStreamsResponse_mediaStreams' - The media streams that you added to the flow.
--
-- 'httpStatus', 'addFlowMediaStreamsResponse_httpStatus' - The response's http status code.
newAddFlowMediaStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddFlowMediaStreamsResponse
newAddFlowMediaStreamsResponse pHttpStatus_ =
  AddFlowMediaStreamsResponse'
    { flowArn =
        Prelude.Nothing,
      mediaStreams = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the flow that you added media streams to.
addFlowMediaStreamsResponse_flowArn :: Lens.Lens' AddFlowMediaStreamsResponse (Prelude.Maybe Prelude.Text)
addFlowMediaStreamsResponse_flowArn = Lens.lens (\AddFlowMediaStreamsResponse' {flowArn} -> flowArn) (\s@AddFlowMediaStreamsResponse' {} a -> s {flowArn = a} :: AddFlowMediaStreamsResponse)

-- | The media streams that you added to the flow.
addFlowMediaStreamsResponse_mediaStreams :: Lens.Lens' AddFlowMediaStreamsResponse (Prelude.Maybe [MediaStream])
addFlowMediaStreamsResponse_mediaStreams = Lens.lens (\AddFlowMediaStreamsResponse' {mediaStreams} -> mediaStreams) (\s@AddFlowMediaStreamsResponse' {} a -> s {mediaStreams = a} :: AddFlowMediaStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addFlowMediaStreamsResponse_httpStatus :: Lens.Lens' AddFlowMediaStreamsResponse Prelude.Int
addFlowMediaStreamsResponse_httpStatus = Lens.lens (\AddFlowMediaStreamsResponse' {httpStatus} -> httpStatus) (\s@AddFlowMediaStreamsResponse' {} a -> s {httpStatus = a} :: AddFlowMediaStreamsResponse)

instance Prelude.NFData AddFlowMediaStreamsResponse where
  rnf AddFlowMediaStreamsResponse' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf mediaStreams
      `Prelude.seq` Prelude.rnf httpStatus
