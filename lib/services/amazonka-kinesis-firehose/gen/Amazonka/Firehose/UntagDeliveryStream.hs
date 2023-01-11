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
-- Module      : Amazonka.Firehose.UntagDeliveryStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified delivery stream. Removed tags are
-- deleted, and you can\'t recover them after this operation successfully
-- completes.
--
-- If you specify a tag that doesn\'t exist, the operation ignores it.
--
-- This operation has a limit of five transactions per second per account.
module Amazonka.Firehose.UntagDeliveryStream
  ( -- * Creating a Request
    UntagDeliveryStream (..),
    newUntagDeliveryStream,

    -- * Request Lenses
    untagDeliveryStream_deliveryStreamName,
    untagDeliveryStream_tagKeys,

    -- * Destructuring the Response
    UntagDeliveryStreamResponse (..),
    newUntagDeliveryStreamResponse,

    -- * Response Lenses
    untagDeliveryStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagDeliveryStream' smart constructor.
data UntagDeliveryStream = UntagDeliveryStream'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Prelude.Text,
    -- | A list of tag keys. Each corresponding tag is removed from the delivery
    -- stream.
    tagKeys :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'untagDeliveryStream_deliveryStreamName' - The name of the delivery stream.
--
-- 'tagKeys', 'untagDeliveryStream_tagKeys' - A list of tag keys. Each corresponding tag is removed from the delivery
-- stream.
newUntagDeliveryStream ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  UntagDeliveryStream
newUntagDeliveryStream pDeliveryStreamName_ pTagKeys_ =
  UntagDeliveryStream'
    { deliveryStreamName =
        pDeliveryStreamName_,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | The name of the delivery stream.
untagDeliveryStream_deliveryStreamName :: Lens.Lens' UntagDeliveryStream Prelude.Text
untagDeliveryStream_deliveryStreamName = Lens.lens (\UntagDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@UntagDeliveryStream' {} a -> s {deliveryStreamName = a} :: UntagDeliveryStream)

-- | A list of tag keys. Each corresponding tag is removed from the delivery
-- stream.
untagDeliveryStream_tagKeys :: Lens.Lens' UntagDeliveryStream (Prelude.NonEmpty Prelude.Text)
untagDeliveryStream_tagKeys = Lens.lens (\UntagDeliveryStream' {tagKeys} -> tagKeys) (\s@UntagDeliveryStream' {} a -> s {tagKeys = a} :: UntagDeliveryStream) Prelude.. Lens.coerced

instance Core.AWSRequest UntagDeliveryStream where
  type
    AWSResponse UntagDeliveryStream =
      UntagDeliveryStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagDeliveryStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagDeliveryStream where
  hashWithSalt _salt UntagDeliveryStream' {..} =
    _salt `Prelude.hashWithSalt` deliveryStreamName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagDeliveryStream where
  rnf UntagDeliveryStream' {..} =
    Prelude.rnf deliveryStreamName
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagDeliveryStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Firehose_20150804.UntagDeliveryStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UntagDeliveryStream where
  toJSON UntagDeliveryStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryStreamName" Data..= deliveryStreamName),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath UntagDeliveryStream where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagDeliveryStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagDeliveryStreamResponse' smart constructor.
data UntagDeliveryStreamResponse = UntagDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'untagDeliveryStreamResponse_httpStatus' - The response's http status code.
newUntagDeliveryStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UntagDeliveryStreamResponse
newUntagDeliveryStreamResponse pHttpStatus_ =
  UntagDeliveryStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
untagDeliveryStreamResponse_httpStatus :: Lens.Lens' UntagDeliveryStreamResponse Prelude.Int
untagDeliveryStreamResponse_httpStatus = Lens.lens (\UntagDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@UntagDeliveryStreamResponse' {} a -> s {httpStatus = a} :: UntagDeliveryStreamResponse)

instance Prelude.NFData UntagDeliveryStreamResponse where
  rnf UntagDeliveryStreamResponse' {..} =
    Prelude.rnf httpStatus
