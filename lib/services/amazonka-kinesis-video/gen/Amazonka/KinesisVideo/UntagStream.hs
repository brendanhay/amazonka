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
-- Module      : Amazonka.KinesisVideo.UntagStream
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.KinesisVideo.UntagStream
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagStream' smart constructor.
data UntagStream = UntagStream'
  { -- | The Amazon Resource Name (ARN) of the stream that you want to remove
    -- tags from.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream that you want to remove tags from.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | A list of the keys of the tags that you want to remove.
    tagKeyList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  UntagStream
newUntagStream pTagKeyList_ =
  UntagStream'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      tagKeyList = Lens.coerced Lens.# pTagKeyList_
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to remove
-- tags from.
untagStream_streamARN :: Lens.Lens' UntagStream (Prelude.Maybe Prelude.Text)
untagStream_streamARN = Lens.lens (\UntagStream' {streamARN} -> streamARN) (\s@UntagStream' {} a -> s {streamARN = a} :: UntagStream)

-- | The name of the stream that you want to remove tags from.
untagStream_streamName :: Lens.Lens' UntagStream (Prelude.Maybe Prelude.Text)
untagStream_streamName = Lens.lens (\UntagStream' {streamName} -> streamName) (\s@UntagStream' {} a -> s {streamName = a} :: UntagStream)

-- | A list of the keys of the tags that you want to remove.
untagStream_tagKeyList :: Lens.Lens' UntagStream (Prelude.NonEmpty Prelude.Text)
untagStream_tagKeyList = Lens.lens (\UntagStream' {tagKeyList} -> tagKeyList) (\s@UntagStream' {} a -> s {tagKeyList = a} :: UntagStream) Prelude.. Lens.coerced

instance Core.AWSRequest UntagStream where
  type AWSResponse UntagStream = UntagStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagStream where
  hashWithSalt _salt UntagStream' {..} =
    _salt `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` tagKeyList

instance Prelude.NFData UntagStream where
  rnf UntagStream' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf tagKeyList

instance Data.ToHeaders UntagStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UntagStream where
  toJSON UntagStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just ("TagKeyList" Data..= tagKeyList)
          ]
      )

instance Data.ToPath UntagStream where
  toPath = Prelude.const "/untagStream"

instance Data.ToQuery UntagStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagStreamResponse' smart constructor.
data UntagStreamResponse = UntagStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UntagStreamResponse
newUntagStreamResponse pHttpStatus_ =
  UntagStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
untagStreamResponse_httpStatus :: Lens.Lens' UntagStreamResponse Prelude.Int
untagStreamResponse_httpStatus = Lens.lens (\UntagStreamResponse' {httpStatus} -> httpStatus) (\s@UntagStreamResponse' {} a -> s {httpStatus = a} :: UntagStreamResponse)

instance Prelude.NFData UntagStreamResponse where
  rnf UntagStreamResponse' {..} = Prelude.rnf httpStatus
