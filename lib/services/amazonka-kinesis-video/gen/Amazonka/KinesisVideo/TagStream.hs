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
-- Module      : Amazonka.KinesisVideo.TagStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a stream. A /tag/ is a key-value pair (the
-- value is optional) that you can define and assign to Amazon Web Services
-- resources. If you specify a tag that already exists, the tag value is
-- replaced with the value that you specify in the request. For more
-- information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /Billing and Cost Management and Cost Management User Guide/.
--
-- You must provide either the @StreamName@ or the @StreamARN@.
--
-- This operation requires permission for the @KinesisVideo:TagStream@
-- action.
--
-- A Kinesis video stream can support up to 50 tags.
module Amazonka.KinesisVideo.TagStream
  ( -- * Creating a Request
    TagStream (..),
    newTagStream,

    -- * Request Lenses
    tagStream_streamARN,
    tagStream_streamName,
    tagStream_tags,

    -- * Destructuring the Response
    TagStreamResponse (..),
    newTagStreamResponse,

    -- * Response Lenses
    tagStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagStream' smart constructor.
data TagStream = TagStream'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to add the
    -- tag or tags to.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream that you want to add the tag or tags to.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to associate with the specified stream. Each tag is a
    -- key-value pair (the value is optional).
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'tagStream_streamARN' - The Amazon Resource Name (ARN) of the resource that you want to add the
-- tag or tags to.
--
-- 'streamName', 'tagStream_streamName' - The name of the stream that you want to add the tag or tags to.
--
-- 'tags', 'tagStream_tags' - A list of tags to associate with the specified stream. Each tag is a
-- key-value pair (the value is optional).
newTagStream ::
  TagStream
newTagStream =
  TagStream'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource that you want to add the
-- tag or tags to.
tagStream_streamARN :: Lens.Lens' TagStream (Prelude.Maybe Prelude.Text)
tagStream_streamARN = Lens.lens (\TagStream' {streamARN} -> streamARN) (\s@TagStream' {} a -> s {streamARN = a} :: TagStream)

-- | The name of the stream that you want to add the tag or tags to.
tagStream_streamName :: Lens.Lens' TagStream (Prelude.Maybe Prelude.Text)
tagStream_streamName = Lens.lens (\TagStream' {streamName} -> streamName) (\s@TagStream' {} a -> s {streamName = a} :: TagStream)

-- | A list of tags to associate with the specified stream. Each tag is a
-- key-value pair (the value is optional).
tagStream_tags :: Lens.Lens' TagStream (Prelude.HashMap Prelude.Text Prelude.Text)
tagStream_tags = Lens.lens (\TagStream' {tags} -> tags) (\s@TagStream' {} a -> s {tags = a} :: TagStream) Prelude.. Lens.coerced

instance Core.AWSRequest TagStream where
  type AWSResponse TagStream = TagStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagStream where
  hashWithSalt _salt TagStream' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagStream where
  rnf TagStream' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON TagStream where
  toJSON TagStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath TagStream where
  toPath = Prelude.const "/tagStream"

instance Data.ToQuery TagStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagStreamResponse' smart constructor.
data TagStreamResponse = TagStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'tagStreamResponse_httpStatus' - The response's http status code.
newTagStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TagStreamResponse
newTagStreamResponse pHttpStatus_ =
  TagStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
tagStreamResponse_httpStatus :: Lens.Lens' TagStreamResponse Prelude.Int
tagStreamResponse_httpStatus = Lens.lens (\TagStreamResponse' {httpStatus} -> httpStatus) (\s@TagStreamResponse' {} a -> s {httpStatus = a} :: TagStreamResponse)

instance Prelude.NFData TagStreamResponse where
  rnf TagStreamResponse' {..} = Prelude.rnf httpStatus
