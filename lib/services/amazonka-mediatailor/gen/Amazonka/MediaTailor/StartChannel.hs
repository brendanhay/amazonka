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
-- Module      : Amazonka.MediaTailor.StartChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a channel. For information about MediaTailor channels, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-channels.html Working with channels>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.StartChannel
  ( -- * Creating a Request
    StartChannel (..),
    newStartChannel,

    -- * Request Lenses
    startChannel_channelName,

    -- * Destructuring the Response
    StartChannelResponse (..),
    newStartChannelResponse,

    -- * Response Lenses
    startChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartChannel' smart constructor.
data StartChannel = StartChannel'
  { -- | The name of the channel.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'startChannel_channelName' - The name of the channel.
newStartChannel ::
  -- | 'channelName'
  Prelude.Text ->
  StartChannel
newStartChannel pChannelName_ =
  StartChannel' {channelName = pChannelName_}

-- | The name of the channel.
startChannel_channelName :: Lens.Lens' StartChannel Prelude.Text
startChannel_channelName = Lens.lens (\StartChannel' {channelName} -> channelName) (\s@StartChannel' {} a -> s {channelName = a} :: StartChannel)

instance Core.AWSRequest StartChannel where
  type AWSResponse StartChannel = StartChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartChannel where
  hashWithSalt _salt StartChannel' {..} =
    _salt `Prelude.hashWithSalt` channelName

instance Prelude.NFData StartChannel where
  rnf StartChannel' {..} = Prelude.rnf channelName

instance Data.ToHeaders StartChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartChannel where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartChannel where
  toPath StartChannel' {..} =
    Prelude.mconcat
      ["/channel/", Data.toBS channelName, "/start"]

instance Data.ToQuery StartChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartChannelResponse' smart constructor.
data StartChannelResponse = StartChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startChannelResponse_httpStatus' - The response's http status code.
newStartChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartChannelResponse
newStartChannelResponse pHttpStatus_ =
  StartChannelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startChannelResponse_httpStatus :: Lens.Lens' StartChannelResponse Prelude.Int
startChannelResponse_httpStatus = Lens.lens (\StartChannelResponse' {httpStatus} -> httpStatus) (\s@StartChannelResponse' {} a -> s {httpStatus = a} :: StartChannelResponse)

instance Prelude.NFData StartChannelResponse where
  rnf StartChannelResponse' {..} =
    Prelude.rnf httpStatus
