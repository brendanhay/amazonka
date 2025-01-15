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
-- Module      : Amazonka.MediaConnect.RemoveFlowMediaStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a media stream from a flow. This action is only available if the
-- media stream is not associated with a source or output.
module Amazonka.MediaConnect.RemoveFlowMediaStream
  ( -- * Creating a Request
    RemoveFlowMediaStream (..),
    newRemoveFlowMediaStream,

    -- * Request Lenses
    removeFlowMediaStream_flowArn,
    removeFlowMediaStream_mediaStreamName,

    -- * Destructuring the Response
    RemoveFlowMediaStreamResponse (..),
    newRemoveFlowMediaStreamResponse,

    -- * Response Lenses
    removeFlowMediaStreamResponse_flowArn,
    removeFlowMediaStreamResponse_mediaStreamName,
    removeFlowMediaStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveFlowMediaStream' smart constructor.
data RemoveFlowMediaStream = RemoveFlowMediaStream'
  { -- | The Amazon Resource Name (ARN) of the flow.
    flowArn :: Prelude.Text,
    -- | The name of the media stream that you want to remove.
    mediaStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowMediaStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowMediaStream_flowArn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'mediaStreamName', 'removeFlowMediaStream_mediaStreamName' - The name of the media stream that you want to remove.
newRemoveFlowMediaStream ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'mediaStreamName'
  Prelude.Text ->
  RemoveFlowMediaStream
newRemoveFlowMediaStream pFlowArn_ pMediaStreamName_ =
  RemoveFlowMediaStream'
    { flowArn = pFlowArn_,
      mediaStreamName = pMediaStreamName_
    }

-- | The Amazon Resource Name (ARN) of the flow.
removeFlowMediaStream_flowArn :: Lens.Lens' RemoveFlowMediaStream Prelude.Text
removeFlowMediaStream_flowArn = Lens.lens (\RemoveFlowMediaStream' {flowArn} -> flowArn) (\s@RemoveFlowMediaStream' {} a -> s {flowArn = a} :: RemoveFlowMediaStream)

-- | The name of the media stream that you want to remove.
removeFlowMediaStream_mediaStreamName :: Lens.Lens' RemoveFlowMediaStream Prelude.Text
removeFlowMediaStream_mediaStreamName = Lens.lens (\RemoveFlowMediaStream' {mediaStreamName} -> mediaStreamName) (\s@RemoveFlowMediaStream' {} a -> s {mediaStreamName = a} :: RemoveFlowMediaStream)

instance Core.AWSRequest RemoveFlowMediaStream where
  type
    AWSResponse RemoveFlowMediaStream =
      RemoveFlowMediaStreamResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveFlowMediaStreamResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "mediaStreamName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveFlowMediaStream where
  hashWithSalt _salt RemoveFlowMediaStream' {..} =
    _salt
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` mediaStreamName

instance Prelude.NFData RemoveFlowMediaStream where
  rnf RemoveFlowMediaStream' {..} =
    Prelude.rnf flowArn `Prelude.seq`
      Prelude.rnf mediaStreamName

instance Data.ToHeaders RemoveFlowMediaStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveFlowMediaStream where
  toPath RemoveFlowMediaStream' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Data.toBS flowArn,
        "/mediaStreams/",
        Data.toBS mediaStreamName
      ]

instance Data.ToQuery RemoveFlowMediaStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveFlowMediaStreamResponse' smart constructor.
data RemoveFlowMediaStreamResponse = RemoveFlowMediaStreamResponse'
  { -- | The Amazon Resource Name (ARN) of the flow.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the media stream that was removed.
    mediaStreamName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveFlowMediaStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'removeFlowMediaStreamResponse_flowArn' - The Amazon Resource Name (ARN) of the flow.
--
-- 'mediaStreamName', 'removeFlowMediaStreamResponse_mediaStreamName' - The name of the media stream that was removed.
--
-- 'httpStatus', 'removeFlowMediaStreamResponse_httpStatus' - The response's http status code.
newRemoveFlowMediaStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveFlowMediaStreamResponse
newRemoveFlowMediaStreamResponse pHttpStatus_ =
  RemoveFlowMediaStreamResponse'
    { flowArn =
        Prelude.Nothing,
      mediaStreamName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the flow.
removeFlowMediaStreamResponse_flowArn :: Lens.Lens' RemoveFlowMediaStreamResponse (Prelude.Maybe Prelude.Text)
removeFlowMediaStreamResponse_flowArn = Lens.lens (\RemoveFlowMediaStreamResponse' {flowArn} -> flowArn) (\s@RemoveFlowMediaStreamResponse' {} a -> s {flowArn = a} :: RemoveFlowMediaStreamResponse)

-- | The name of the media stream that was removed.
removeFlowMediaStreamResponse_mediaStreamName :: Lens.Lens' RemoveFlowMediaStreamResponse (Prelude.Maybe Prelude.Text)
removeFlowMediaStreamResponse_mediaStreamName = Lens.lens (\RemoveFlowMediaStreamResponse' {mediaStreamName} -> mediaStreamName) (\s@RemoveFlowMediaStreamResponse' {} a -> s {mediaStreamName = a} :: RemoveFlowMediaStreamResponse)

-- | The response's http status code.
removeFlowMediaStreamResponse_httpStatus :: Lens.Lens' RemoveFlowMediaStreamResponse Prelude.Int
removeFlowMediaStreamResponse_httpStatus = Lens.lens (\RemoveFlowMediaStreamResponse' {httpStatus} -> httpStatus) (\s@RemoveFlowMediaStreamResponse' {} a -> s {httpStatus = a} :: RemoveFlowMediaStreamResponse)

instance Prelude.NFData RemoveFlowMediaStreamResponse where
  rnf RemoveFlowMediaStreamResponse' {..} =
    Prelude.rnf flowArn `Prelude.seq`
      Prelude.rnf mediaStreamName `Prelude.seq`
        Prelude.rnf httpStatus
