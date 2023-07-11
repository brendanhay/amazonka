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
-- Module      : Amazonka.IVS.GetChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the channel configuration for the specified channel ARN. See also
-- BatchGetChannel.
module Amazonka.IVS.GetChannel
  ( -- * Creating a Request
    GetChannel (..),
    newGetChannel,

    -- * Request Lenses
    getChannel_arn,

    -- * Destructuring the Response
    GetChannelResponse (..),
    newGetChannelResponse,

    -- * Response Lenses
    getChannelResponse_channel,
    getChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannel' smart constructor.
data GetChannel = GetChannel'
  { -- | ARN of the channel for which the configuration is to be retrieved.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getChannel_arn' - ARN of the channel for which the configuration is to be retrieved.
newGetChannel ::
  -- | 'arn'
  Prelude.Text ->
  GetChannel
newGetChannel pArn_ = GetChannel' {arn = pArn_}

-- | ARN of the channel for which the configuration is to be retrieved.
getChannel_arn :: Lens.Lens' GetChannel Prelude.Text
getChannel_arn = Lens.lens (\GetChannel' {arn} -> arn) (\s@GetChannel' {} a -> s {arn = a} :: GetChannel)

instance Core.AWSRequest GetChannel where
  type AWSResponse GetChannel = GetChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelResponse'
            Prelude.<$> (x Data..?> "channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannel where
  hashWithSalt _salt GetChannel' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetChannel where
  rnf GetChannel' {..} = Prelude.rnf arn

instance Data.ToHeaders GetChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetChannel where
  toJSON GetChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetChannel where
  toPath = Prelude.const "/GetChannel"

instance Data.ToQuery GetChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelResponse' smart constructor.
data GetChannelResponse = GetChannelResponse'
  { channel :: Prelude.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'getChannelResponse_channel' -
--
-- 'httpStatus', 'getChannelResponse_httpStatus' - The response's http status code.
newGetChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelResponse
newGetChannelResponse pHttpStatus_ =
  GetChannelResponse'
    { channel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

getChannelResponse_channel :: Lens.Lens' GetChannelResponse (Prelude.Maybe Channel)
getChannelResponse_channel = Lens.lens (\GetChannelResponse' {channel} -> channel) (\s@GetChannelResponse' {} a -> s {channel = a} :: GetChannelResponse)

-- | The response's http status code.
getChannelResponse_httpStatus :: Lens.Lens' GetChannelResponse Prelude.Int
getChannelResponse_httpStatus = Lens.lens (\GetChannelResponse' {httpStatus} -> httpStatus) (\s@GetChannelResponse' {} a -> s {httpStatus = a} :: GetChannelResponse)

instance Prelude.NFData GetChannelResponse where
  rnf GetChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf httpStatus
