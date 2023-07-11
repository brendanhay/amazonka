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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified Kinesis data stream. Removed tags are
-- deleted and cannot be recovered after this operation successfully
-- completes.
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
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
    removeTagsFromStream_streamARN,
    removeTagsFromStream_streamName,
    removeTagsFromStream_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromStreamResponse (..),
    newRemoveTagsFromStreamResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for @RemoveTagsFromStream@.
--
-- /See:/ 'newRemoveTagsFromStream' smart constructor.
data RemoveTagsFromStream = RemoveTagsFromStream'
  { -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream.
    streamName :: Prelude.Maybe Prelude.Text,
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
-- 'streamARN', 'removeTagsFromStream_streamARN' - The ARN of the stream.
--
-- 'streamName', 'removeTagsFromStream_streamName' - The name of the stream.
--
-- 'tagKeys', 'removeTagsFromStream_tagKeys' - A list of tag keys. Each corresponding tag is removed from the stream.
newRemoveTagsFromStream ::
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  RemoveTagsFromStream
newRemoveTagsFromStream pTagKeys_ =
  RemoveTagsFromStream'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | The ARN of the stream.
removeTagsFromStream_streamARN :: Lens.Lens' RemoveTagsFromStream (Prelude.Maybe Prelude.Text)
removeTagsFromStream_streamARN = Lens.lens (\RemoveTagsFromStream' {streamARN} -> streamARN) (\s@RemoveTagsFromStream' {} a -> s {streamARN = a} :: RemoveTagsFromStream)

-- | The name of the stream.
removeTagsFromStream_streamName :: Lens.Lens' RemoveTagsFromStream (Prelude.Maybe Prelude.Text)
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
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromStream where
  rnf RemoveTagsFromStream' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTagsFromStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.RemoveTagsFromStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTagsFromStream where
  toJSON RemoveTagsFromStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTagsFromStream where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTagsFromStream where
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
