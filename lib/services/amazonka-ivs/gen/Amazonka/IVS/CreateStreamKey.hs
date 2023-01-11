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
-- Module      : Amazonka.IVS.CreateStreamKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stream key, used to initiate a stream, for the specified
-- channel ARN.
--
-- Note that CreateChannel creates a stream key. If you subsequently use
-- CreateStreamKey on the same channel, it will fail because a stream key
-- already exists and there is a limit of 1 stream key per channel. To
-- reset the stream key on a channel, use DeleteStreamKey and then
-- CreateStreamKey.
module Amazonka.IVS.CreateStreamKey
  ( -- * Creating a Request
    CreateStreamKey (..),
    newCreateStreamKey,

    -- * Request Lenses
    createStreamKey_tags,
    createStreamKey_channelArn,

    -- * Destructuring the Response
    CreateStreamKeyResponse (..),
    newCreateStreamKeyResponse,

    -- * Response Lenses
    createStreamKeyResponse_streamKey,
    createStreamKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStreamKey' smart constructor.
data CreateStreamKey = CreateStreamKey'
  { -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | ARN of the channel for which to create the stream key.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createStreamKey_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'channelArn', 'createStreamKey_channelArn' - ARN of the channel for which to create the stream key.
newCreateStreamKey ::
  -- | 'channelArn'
  Prelude.Text ->
  CreateStreamKey
newCreateStreamKey pChannelArn_ =
  CreateStreamKey'
    { tags = Prelude.Nothing,
      channelArn = pChannelArn_
    }

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
createStreamKey_tags :: Lens.Lens' CreateStreamKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStreamKey_tags = Lens.lens (\CreateStreamKey' {tags} -> tags) (\s@CreateStreamKey' {} a -> s {tags = a} :: CreateStreamKey) Prelude.. Lens.mapping Lens.coerced

-- | ARN of the channel for which to create the stream key.
createStreamKey_channelArn :: Lens.Lens' CreateStreamKey Prelude.Text
createStreamKey_channelArn = Lens.lens (\CreateStreamKey' {channelArn} -> channelArn) (\s@CreateStreamKey' {} a -> s {channelArn = a} :: CreateStreamKey)

instance Core.AWSRequest CreateStreamKey where
  type
    AWSResponse CreateStreamKey =
      CreateStreamKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamKeyResponse'
            Prelude.<$> (x Data..?> "streamKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamKey where
  hashWithSalt _salt CreateStreamKey' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` channelArn

instance Prelude.NFData CreateStreamKey where
  rnf CreateStreamKey' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf channelArn

instance Data.ToHeaders CreateStreamKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStreamKey where
  toJSON CreateStreamKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("channelArn" Data..= channelArn)
          ]
      )

instance Data.ToPath CreateStreamKey where
  toPath = Prelude.const "/CreateStreamKey"

instance Data.ToQuery CreateStreamKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamKeyResponse' smart constructor.
data CreateStreamKeyResponse = CreateStreamKeyResponse'
  { -- | Stream key used to authenticate an RTMPS stream for ingestion.
    streamKey :: Prelude.Maybe StreamKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamKey', 'createStreamKeyResponse_streamKey' - Stream key used to authenticate an RTMPS stream for ingestion.
--
-- 'httpStatus', 'createStreamKeyResponse_httpStatus' - The response's http status code.
newCreateStreamKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamKeyResponse
newCreateStreamKeyResponse pHttpStatus_ =
  CreateStreamKeyResponse'
    { streamKey =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Stream key used to authenticate an RTMPS stream for ingestion.
createStreamKeyResponse_streamKey :: Lens.Lens' CreateStreamKeyResponse (Prelude.Maybe StreamKey)
createStreamKeyResponse_streamKey = Lens.lens (\CreateStreamKeyResponse' {streamKey} -> streamKey) (\s@CreateStreamKeyResponse' {} a -> s {streamKey = a} :: CreateStreamKeyResponse)

-- | The response's http status code.
createStreamKeyResponse_httpStatus :: Lens.Lens' CreateStreamKeyResponse Prelude.Int
createStreamKeyResponse_httpStatus = Lens.lens (\CreateStreamKeyResponse' {httpStatus} -> httpStatus) (\s@CreateStreamKeyResponse' {} a -> s {httpStatus = a} :: CreateStreamKeyResponse)

instance Prelude.NFData CreateStreamKeyResponse where
  rnf CreateStreamKeyResponse' {..} =
    Prelude.rnf streamKey
      `Prelude.seq` Prelude.rnf httpStatus
