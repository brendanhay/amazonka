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
-- Module      : Amazonka.MediaTailor.StopChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a channel. For information about MediaTailor channels, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-channels.html Working with channels>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.StopChannel
  ( -- * Creating a Request
    StopChannel (..),
    newStopChannel,

    -- * Request Lenses
    stopChannel_channelName,

    -- * Destructuring the Response
    StopChannelResponse (..),
    newStopChannelResponse,

    -- * Response Lenses
    stopChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopChannel' smart constructor.
data StopChannel = StopChannel'
  { -- | The name of the channel.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'stopChannel_channelName' - The name of the channel.
newStopChannel ::
  -- | 'channelName'
  Prelude.Text ->
  StopChannel
newStopChannel pChannelName_ =
  StopChannel' {channelName = pChannelName_}

-- | The name of the channel.
stopChannel_channelName :: Lens.Lens' StopChannel Prelude.Text
stopChannel_channelName = Lens.lens (\StopChannel' {channelName} -> channelName) (\s@StopChannel' {} a -> s {channelName = a} :: StopChannel)

instance Core.AWSRequest StopChannel where
  type AWSResponse StopChannel = StopChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopChannel where
  hashWithSalt _salt StopChannel' {..} =
    _salt `Prelude.hashWithSalt` channelName

instance Prelude.NFData StopChannel where
  rnf StopChannel' {..} = Prelude.rnf channelName

instance Data.ToHeaders StopChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopChannel where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopChannel where
  toPath StopChannel' {..} =
    Prelude.mconcat
      ["/channel/", Data.toBS channelName, "/stop"]

instance Data.ToQuery StopChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopChannelResponse' smart constructor.
data StopChannelResponse = StopChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopChannelResponse_httpStatus' - The response's http status code.
newStopChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopChannelResponse
newStopChannelResponse pHttpStatus_ =
  StopChannelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopChannelResponse_httpStatus :: Lens.Lens' StopChannelResponse Prelude.Int
stopChannelResponse_httpStatus = Lens.lens (\StopChannelResponse' {httpStatus} -> httpStatus) (\s@StopChannelResponse' {} a -> s {httpStatus = a} :: StopChannelResponse)

instance Prelude.NFData StopChannelResponse where
  rnf StopChannelResponse' {..} = Prelude.rnf httpStatus
