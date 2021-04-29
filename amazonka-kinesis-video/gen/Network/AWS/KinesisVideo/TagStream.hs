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
-- Module      : Network.AWS.KinesisVideo.TagStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a stream. A /tag/ is a key-value pair (the
-- value is optional) that you can define and assign to AWS resources. If
-- you specify a tag that already exists, the tag value is replaced with
-- the value that you specify in the request. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- You must provide either the @StreamName@ or the @StreamARN@.
--
-- This operation requires permission for the @KinesisVideo:TagStream@
-- action.
--
-- Kinesis video streams support up to 50 tags.
module Network.AWS.KinesisVideo.TagStream
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

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
tagStream_tags = Lens.lens (\TagStream' {tags} -> tags) (\s@TagStream' {} a -> s {tags = a} :: TagStream) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagStream where
  type Rs TagStream = TagStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagStream

instance Prelude.NFData TagStream

instance Prelude.ToHeaders TagStream where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON TagStream where
  toJSON TagStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StreamARN" Prelude..=) Prelude.<$> streamARN,
            ("StreamName" Prelude..=) Prelude.<$> streamName,
            Prelude.Just ("Tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath TagStream where
  toPath = Prelude.const "/tagStream"

instance Prelude.ToQuery TagStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagStreamResponse' smart constructor.
data TagStreamResponse = TagStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData TagStreamResponse
