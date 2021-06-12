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
-- Module      : Network.AWS.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified Kinesis data stream. Removed tags are
-- deleted and cannot be recovered after this operation successfully
-- completes.
--
-- If you specify a tag that does not exist, it is ignored.
--
-- RemoveTagsFromStream has a limit of five transactions per second per
-- account.
module Network.AWS.Kinesis.RemoveTagsFromStream
  ( -- * Creating a Request
    RemoveTagsFromStream (..),
    newRemoveTagsFromStream,

    -- * Request Lenses
    removeTagsFromStream_streamName,
    removeTagsFromStream_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromStreamResponse (..),
    newRemoveTagsFromStreamResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @RemoveTagsFromStream@.
--
-- /See:/ 'newRemoveTagsFromStream' smart constructor.
data RemoveTagsFromStream = RemoveTagsFromStream'
  { -- | The name of the stream.
    streamName :: Core.Text,
    -- | A list of tag keys. Each corresponding tag is removed from the stream.
    tagKeys :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveTagsFromStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'removeTagsFromStream_streamName' - The name of the stream.
--
-- 'tagKeys', 'removeTagsFromStream_tagKeys' - A list of tag keys. Each corresponding tag is removed from the stream.
newRemoveTagsFromStream ::
  -- | 'streamName'
  Core.Text ->
  -- | 'tagKeys'
  Core.NonEmpty Core.Text ->
  RemoveTagsFromStream
newRemoveTagsFromStream pStreamName_ pTagKeys_ =
  RemoveTagsFromStream'
    { streamName = pStreamName_,
      tagKeys = Lens._Coerce Lens.# pTagKeys_
    }

-- | The name of the stream.
removeTagsFromStream_streamName :: Lens.Lens' RemoveTagsFromStream Core.Text
removeTagsFromStream_streamName = Lens.lens (\RemoveTagsFromStream' {streamName} -> streamName) (\s@RemoveTagsFromStream' {} a -> s {streamName = a} :: RemoveTagsFromStream)

-- | A list of tag keys. Each corresponding tag is removed from the stream.
removeTagsFromStream_tagKeys :: Lens.Lens' RemoveTagsFromStream (Core.NonEmpty Core.Text)
removeTagsFromStream_tagKeys = Lens.lens (\RemoveTagsFromStream' {tagKeys} -> tagKeys) (\s@RemoveTagsFromStream' {} a -> s {tagKeys = a} :: RemoveTagsFromStream) Core.. Lens._Coerce

instance Core.AWSRequest RemoveTagsFromStream where
  type
    AWSResponse RemoveTagsFromStream =
      RemoveTagsFromStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RemoveTagsFromStreamResponse'

instance Core.Hashable RemoveTagsFromStream

instance Core.NFData RemoveTagsFromStream

instance Core.ToHeaders RemoveTagsFromStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.RemoveTagsFromStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveTagsFromStream where
  toJSON RemoveTagsFromStream' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath RemoveTagsFromStream where
  toPath = Core.const "/"

instance Core.ToQuery RemoveTagsFromStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveTagsFromStreamResponse' smart constructor.
data RemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveTagsFromStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsFromStreamResponse ::
  RemoveTagsFromStreamResponse
newRemoveTagsFromStreamResponse =
  RemoveTagsFromStreamResponse'

instance Core.NFData RemoveTagsFromStreamResponse
