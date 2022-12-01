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
-- Module      : Amazonka.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Kinesis.RemoveTagsFromStream
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for @RemoveTagsFromStream@.
--
-- /See:/ 'newRemoveTagsFromStream' smart constructor.
data RemoveTagsFromStream = RemoveTagsFromStream'
  { -- | The name of the stream.
    streamName :: Prelude.Text,
    -- | A list of tag keys. Each corresponding tag is removed from the stream.
    tagKeys :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  RemoveTagsFromStream
newRemoveTagsFromStream pStreamName_ pTagKeys_ =
  RemoveTagsFromStream'
    { streamName = pStreamName_,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | The name of the stream.
removeTagsFromStream_streamName :: Lens.Lens' RemoveTagsFromStream Prelude.Text
removeTagsFromStream_streamName = Lens.lens (\RemoveTagsFromStream' {streamName} -> streamName) (\s@RemoveTagsFromStream' {} a -> s {streamName = a} :: RemoveTagsFromStream)

-- | A list of tag keys. Each corresponding tag is removed from the stream.
removeTagsFromStream_tagKeys :: Lens.Lens' RemoveTagsFromStream (Prelude.NonEmpty Prelude.Text)
removeTagsFromStream_tagKeys = Lens.lens (\RemoveTagsFromStream' {tagKeys} -> tagKeys) (\s@RemoveTagsFromStream' {} a -> s {tagKeys = a} :: RemoveTagsFromStream) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromStream where
  type
    AWSResponse RemoveTagsFromStream =
      RemoveTagsFromStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RemoveTagsFromStreamResponse'

instance Prelude.Hashable RemoveTagsFromStream where
  hashWithSalt _salt RemoveTagsFromStream' {..} =
    _salt `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromStream where
  rnf RemoveTagsFromStream' {..} =
    Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf tagKeys

instance Core.ToHeaders RemoveTagsFromStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.RemoveTagsFromStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveTagsFromStream where
  toJSON RemoveTagsFromStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Core..= streamName),
            Prelude.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath RemoveTagsFromStream where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTagsFromStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromStreamResponse' smart constructor.
data RemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsFromStreamResponse ::
  RemoveTagsFromStreamResponse
newRemoveTagsFromStreamResponse =
  RemoveTagsFromStreamResponse'

instance Prelude.NFData RemoveTagsFromStreamResponse where
  rnf _ = ()
