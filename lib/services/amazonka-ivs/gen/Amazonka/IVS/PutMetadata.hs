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
-- Module      : Amazonka.IVS.PutMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts metadata into the active stream of the specified channel. At
-- most 5 requests per second per channel are allowed, each with a maximum
-- 1 KB payload. (If 5 TPS is not sufficient for your needs, we recommend
-- batching your data into a single PutMetadata call.) At most 155 requests
-- per second per account are allowed. Also see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/metadata.html Embedding Metadata within a Video Stream>
-- in the /Amazon IVS User Guide/.
module Amazonka.IVS.PutMetadata
  ( -- * Creating a Request
    PutMetadata (..),
    newPutMetadata,

    -- * Request Lenses
    putMetadata_channelArn,
    putMetadata_metadata,

    -- * Destructuring the Response
    PutMetadataResponse (..),
    newPutMetadataResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetadata' smart constructor.
data PutMetadata = PutMetadata'
  { -- | ARN of the channel into which metadata is inserted. This channel must
    -- have an active stream.
    channelArn :: Prelude.Text,
    -- | Metadata to insert into the stream. Maximum: 1 KB per request.
    metadata :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'putMetadata_channelArn' - ARN of the channel into which metadata is inserted. This channel must
-- have an active stream.
--
-- 'metadata', 'putMetadata_metadata' - Metadata to insert into the stream. Maximum: 1 KB per request.
newPutMetadata ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'metadata'
  Prelude.Text ->
  PutMetadata
newPutMetadata pChannelArn_ pMetadata_ =
  PutMetadata'
    { channelArn = pChannelArn_,
      metadata = Data._Sensitive Lens.# pMetadata_
    }

-- | ARN of the channel into which metadata is inserted. This channel must
-- have an active stream.
putMetadata_channelArn :: Lens.Lens' PutMetadata Prelude.Text
putMetadata_channelArn = Lens.lens (\PutMetadata' {channelArn} -> channelArn) (\s@PutMetadata' {} a -> s {channelArn = a} :: PutMetadata)

-- | Metadata to insert into the stream. Maximum: 1 KB per request.
putMetadata_metadata :: Lens.Lens' PutMetadata Prelude.Text
putMetadata_metadata = Lens.lens (\PutMetadata' {metadata} -> metadata) (\s@PutMetadata' {} a -> s {metadata = a} :: PutMetadata) Prelude.. Data._Sensitive

instance Core.AWSRequest PutMetadata where
  type AWSResponse PutMetadata = PutMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull PutMetadataResponse'

instance Prelude.Hashable PutMetadata where
  hashWithSalt _salt PutMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData PutMetadata where
  rnf PutMetadata' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf metadata

instance Data.ToHeaders PutMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutMetadata where
  toJSON PutMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("channelArn" Data..= channelArn),
            Prelude.Just ("metadata" Data..= metadata)
          ]
      )

instance Data.ToPath PutMetadata where
  toPath = Prelude.const "/PutMetadata"

instance Data.ToQuery PutMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMetadataResponse' smart constructor.
data PutMetadataResponse = PutMetadataResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetadataResponse ::
  PutMetadataResponse
newPutMetadataResponse = PutMetadataResponse'

instance Prelude.NFData PutMetadataResponse where
  rnf _ = ()
