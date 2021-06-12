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
-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified Kinesis data stream. Each time
-- you invoke this operation, you can specify up to 10 tags. If you want to
-- add more than 10 tags to your stream, you can invoke this operation
-- multiple times. In total, each stream can have up to 50 tags.
--
-- If tags have already been assigned to the stream, @AddTagsToStream@
-- overwrites any existing tags that correspond to the specified tag keys.
--
-- AddTagsToStream has a limit of five transactions per second per account.
module Network.AWS.Kinesis.AddTagsToStream
  ( -- * Creating a Request
    AddTagsToStream (..),
    newAddTagsToStream,

    -- * Request Lenses
    addTagsToStream_streamName,
    addTagsToStream_tags,

    -- * Destructuring the Response
    AddTagsToStreamResponse (..),
    newAddTagsToStreamResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @AddTagsToStream@.
--
-- /See:/ 'newAddTagsToStream' smart constructor.
data AddTagsToStream = AddTagsToStream'
  { -- | The name of the stream.
    streamName :: Core.Text,
    -- | A set of up to 10 key-value pairs to use to create the tags.
    tags :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'addTagsToStream_streamName' - The name of the stream.
--
-- 'tags', 'addTagsToStream_tags' - A set of up to 10 key-value pairs to use to create the tags.
newAddTagsToStream ::
  -- | 'streamName'
  Core.Text ->
  AddTagsToStream
newAddTagsToStream pStreamName_ =
  AddTagsToStream'
    { streamName = pStreamName_,
      tags = Core.mempty
    }

-- | The name of the stream.
addTagsToStream_streamName :: Lens.Lens' AddTagsToStream Core.Text
addTagsToStream_streamName = Lens.lens (\AddTagsToStream' {streamName} -> streamName) (\s@AddTagsToStream' {} a -> s {streamName = a} :: AddTagsToStream)

-- | A set of up to 10 key-value pairs to use to create the tags.
addTagsToStream_tags :: Lens.Lens' AddTagsToStream (Core.HashMap Core.Text Core.Text)
addTagsToStream_tags = Lens.lens (\AddTagsToStream' {tags} -> tags) (\s@AddTagsToStream' {} a -> s {tags = a} :: AddTagsToStream) Core.. Lens._Coerce

instance Core.AWSRequest AddTagsToStream where
  type
    AWSResponse AddTagsToStream =
      AddTagsToStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AddTagsToStreamResponse'

instance Core.Hashable AddTagsToStream

instance Core.NFData AddTagsToStream

instance Core.ToHeaders AddTagsToStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.AddTagsToStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddTagsToStream where
  toJSON AddTagsToStream' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath AddTagsToStream where
  toPath = Core.const "/"

instance Core.ToQuery AddTagsToStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddTagsToStreamResponse' smart constructor.
data AddTagsToStreamResponse = AddTagsToStreamResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToStreamResponse ::
  AddTagsToStreamResponse
newAddTagsToStreamResponse = AddTagsToStreamResponse'

instance Core.NFData AddTagsToStreamResponse
