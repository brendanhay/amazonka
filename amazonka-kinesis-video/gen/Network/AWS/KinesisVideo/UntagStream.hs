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
-- Module      : Network.AWS.KinesisVideo.UntagStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a stream. In the request, specify only a
-- tag key or keys; don\'t specify the value. If you specify a tag key that
-- does not exist, it\'s ignored.
--
-- In the request, you must provide the @StreamName@ or @StreamARN@.
module Network.AWS.KinesisVideo.UntagStream
  ( -- * Creating a Request
    UntagStream (..),
    newUntagStream,

    -- * Request Lenses
    untagStream_streamARN,
    untagStream_streamName,
    untagStream_tagKeyList,

    -- * Destructuring the Response
    UntagStreamResponse (..),
    newUntagStreamResponse,

    -- * Response Lenses
    untagStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagStream' smart constructor.
data UntagStream = UntagStream'
  { -- | The Amazon Resource Name (ARN) of the stream that you want to remove
    -- tags from.
    streamARN :: Core.Maybe Core.Text,
    -- | The name of the stream that you want to remove tags from.
    streamName :: Core.Maybe Core.Text,
    -- | A list of the keys of the tags that you want to remove.
    tagKeyList :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'untagStream_streamARN' - The Amazon Resource Name (ARN) of the stream that you want to remove
-- tags from.
--
-- 'streamName', 'untagStream_streamName' - The name of the stream that you want to remove tags from.
--
-- 'tagKeyList', 'untagStream_tagKeyList' - A list of the keys of the tags that you want to remove.
newUntagStream ::
  -- | 'tagKeyList'
  Core.NonEmpty Core.Text ->
  UntagStream
newUntagStream pTagKeyList_ =
  UntagStream'
    { streamARN = Core.Nothing,
      streamName = Core.Nothing,
      tagKeyList = Lens._Coerce Lens.# pTagKeyList_
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to remove
-- tags from.
untagStream_streamARN :: Lens.Lens' UntagStream (Core.Maybe Core.Text)
untagStream_streamARN = Lens.lens (\UntagStream' {streamARN} -> streamARN) (\s@UntagStream' {} a -> s {streamARN = a} :: UntagStream)

-- | The name of the stream that you want to remove tags from.
untagStream_streamName :: Lens.Lens' UntagStream (Core.Maybe Core.Text)
untagStream_streamName = Lens.lens (\UntagStream' {streamName} -> streamName) (\s@UntagStream' {} a -> s {streamName = a} :: UntagStream)

-- | A list of the keys of the tags that you want to remove.
untagStream_tagKeyList :: Lens.Lens' UntagStream (Core.NonEmpty Core.Text)
untagStream_tagKeyList = Lens.lens (\UntagStream' {tagKeyList} -> tagKeyList) (\s@UntagStream' {} a -> s {tagKeyList = a} :: UntagStream) Core.. Lens._Coerce

instance Core.AWSRequest UntagStream where
  type AWSResponse UntagStream = UntagStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UntagStream

instance Core.NFData UntagStream

instance Core.ToHeaders UntagStream where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UntagStream where
  toJSON UntagStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName,
            Core.Just ("TagKeyList" Core..= tagKeyList)
          ]
      )

instance Core.ToPath UntagStream where
  toPath = Core.const "/untagStream"

instance Core.ToQuery UntagStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUntagStreamResponse' smart constructor.
data UntagStreamResponse = UntagStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'untagStreamResponse_httpStatus' - The response's http status code.
newUntagStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UntagStreamResponse
newUntagStreamResponse pHttpStatus_ =
  UntagStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
untagStreamResponse_httpStatus :: Lens.Lens' UntagStreamResponse Core.Int
untagStreamResponse_httpStatus = Lens.lens (\UntagStreamResponse' {httpStatus} -> httpStatus) (\s@UntagStreamResponse' {} a -> s {httpStatus = a} :: UntagStreamResponse)

instance Core.NFData UntagStreamResponse
