{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisVideo.DeleteStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis video stream and the data contained in the stream.
--
-- This method marks the stream for deletion, and makes the data in the
-- stream inaccessible immediately.
--
-- To ensure that you have the latest version of the stream before deleting
-- it, you can specify the stream version. Kinesis Video Streams assigns a
-- version to each stream. When you update a stream, Kinesis Video Streams
-- assigns a new version number. To get the latest stream version, use the
-- @DescribeStream@ API.
--
-- This operation requires permission for the @KinesisVideo:DeleteStream@
-- action.
module Network.AWS.KinesisVideo.DeleteStream
  ( -- * Creating a Request
    DeleteStream (..),
    newDeleteStream,

    -- * Request Lenses
    deleteStream_currentVersion,
    deleteStream_streamARN,

    -- * Destructuring the Response
    DeleteStreamResponse (..),
    newDeleteStreamResponse,

    -- * Response Lenses
    deleteStreamResponse_httpStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | Optional: The version of the stream that you want to delete.
    --
    -- Specify the version as a safeguard to ensure that your are deleting the
    -- correct stream. To get the stream version, use the @DescribeStream@ API.
    --
    -- If not specified, only the @CreationTime@ is checked before deleting the
    -- stream.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stream that you want to delete.
    streamARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'deleteStream_currentVersion' - Optional: The version of the stream that you want to delete.
--
-- Specify the version as a safeguard to ensure that your are deleting the
-- correct stream. To get the stream version, use the @DescribeStream@ API.
--
-- If not specified, only the @CreationTime@ is checked before deleting the
-- stream.
--
-- 'streamARN', 'deleteStream_streamARN' - The Amazon Resource Name (ARN) of the stream that you want to delete.
newDeleteStream ::
  -- | 'streamARN'
  Prelude.Text ->
  DeleteStream
newDeleteStream pStreamARN_ =
  DeleteStream'
    { currentVersion = Prelude.Nothing,
      streamARN = pStreamARN_
    }

-- | Optional: The version of the stream that you want to delete.
--
-- Specify the version as a safeguard to ensure that your are deleting the
-- correct stream. To get the stream version, use the @DescribeStream@ API.
--
-- If not specified, only the @CreationTime@ is checked before deleting the
-- stream.
deleteStream_currentVersion :: Lens.Lens' DeleteStream (Prelude.Maybe Prelude.Text)
deleteStream_currentVersion = Lens.lens (\DeleteStream' {currentVersion} -> currentVersion) (\s@DeleteStream' {} a -> s {currentVersion = a} :: DeleteStream)

-- | The Amazon Resource Name (ARN) of the stream that you want to delete.
deleteStream_streamARN :: Lens.Lens' DeleteStream Prelude.Text
deleteStream_streamARN = Lens.lens (\DeleteStream' {streamARN} -> streamARN) (\s@DeleteStream' {} a -> s {streamARN = a} :: DeleteStream)

instance Prelude.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStream

instance Prelude.NFData DeleteStream

instance Prelude.ToHeaders DeleteStream where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON DeleteStream where
  toJSON DeleteStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CurrentVersion" Prelude..=)
              Prelude.<$> currentVersion,
            Prelude.Just ("StreamARN" Prelude..= streamARN)
          ]
      )

instance Prelude.ToPath DeleteStream where
  toPath = Prelude.const "/deleteStream"

instance Prelude.ToQuery DeleteStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStreamResponse_httpStatus' - The response's http status code.
newDeleteStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStreamResponse
newDeleteStreamResponse pHttpStatus_ =
  DeleteStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteStreamResponse_httpStatus :: Lens.Lens' DeleteStreamResponse Prelude.Int
deleteStreamResponse_httpStatus = Lens.lens (\DeleteStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteStreamResponse' {} a -> s {httpStatus = a} :: DeleteStreamResponse)

instance Prelude.NFData DeleteStreamResponse
