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
-- Module      : Amazonka.MediaTailor.ConfigureLogsForChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures Amazon CloudWatch log settings for a channel.
module Amazonka.MediaTailor.ConfigureLogsForChannel
  ( -- * Creating a Request
    ConfigureLogsForChannel (..),
    newConfigureLogsForChannel,

    -- * Request Lenses
    configureLogsForChannel_channelName,
    configureLogsForChannel_logTypes,

    -- * Destructuring the Response
    ConfigureLogsForChannelResponse (..),
    newConfigureLogsForChannelResponse,

    -- * Response Lenses
    configureLogsForChannelResponse_channelName,
    configureLogsForChannelResponse_logTypes,
    configureLogsForChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConfigureLogsForChannel' smart constructor.
data ConfigureLogsForChannel = ConfigureLogsForChannel'
  { -- | The name of the channel.
    channelName :: Prelude.Text,
    -- | The types of logs to collect.
    logTypes :: [LogType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureLogsForChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'configureLogsForChannel_channelName' - The name of the channel.
--
-- 'logTypes', 'configureLogsForChannel_logTypes' - The types of logs to collect.
newConfigureLogsForChannel ::
  -- | 'channelName'
  Prelude.Text ->
  ConfigureLogsForChannel
newConfigureLogsForChannel pChannelName_ =
  ConfigureLogsForChannel'
    { channelName =
        pChannelName_,
      logTypes = Prelude.mempty
    }

-- | The name of the channel.
configureLogsForChannel_channelName :: Lens.Lens' ConfigureLogsForChannel Prelude.Text
configureLogsForChannel_channelName = Lens.lens (\ConfigureLogsForChannel' {channelName} -> channelName) (\s@ConfigureLogsForChannel' {} a -> s {channelName = a} :: ConfigureLogsForChannel)

-- | The types of logs to collect.
configureLogsForChannel_logTypes :: Lens.Lens' ConfigureLogsForChannel [LogType]
configureLogsForChannel_logTypes = Lens.lens (\ConfigureLogsForChannel' {logTypes} -> logTypes) (\s@ConfigureLogsForChannel' {} a -> s {logTypes = a} :: ConfigureLogsForChannel) Prelude.. Lens.coerced

instance Core.AWSRequest ConfigureLogsForChannel where
  type
    AWSResponse ConfigureLogsForChannel =
      ConfigureLogsForChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfigureLogsForChannelResponse'
            Prelude.<$> (x Data..?> "ChannelName")
            Prelude.<*> (x Data..?> "LogTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfigureLogsForChannel where
  hashWithSalt _salt ConfigureLogsForChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` logTypes

instance Prelude.NFData ConfigureLogsForChannel where
  rnf ConfigureLogsForChannel' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf logTypes

instance Data.ToHeaders ConfigureLogsForChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfigureLogsForChannel where
  toJSON ConfigureLogsForChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ChannelName" Data..= channelName),
            Prelude.Just ("LogTypes" Data..= logTypes)
          ]
      )

instance Data.ToPath ConfigureLogsForChannel where
  toPath = Prelude.const "/configureLogs/channel"

instance Data.ToQuery ConfigureLogsForChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfigureLogsForChannelResponse' smart constructor.
data ConfigureLogsForChannelResponse = ConfigureLogsForChannelResponse'
  { -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The types of logs collected.
    logTypes :: Prelude.Maybe [LogType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureLogsForChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'configureLogsForChannelResponse_channelName' - The name of the channel.
--
-- 'logTypes', 'configureLogsForChannelResponse_logTypes' - The types of logs collected.
--
-- 'httpStatus', 'configureLogsForChannelResponse_httpStatus' - The response's http status code.
newConfigureLogsForChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfigureLogsForChannelResponse
newConfigureLogsForChannelResponse pHttpStatus_ =
  ConfigureLogsForChannelResponse'
    { channelName =
        Prelude.Nothing,
      logTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the channel.
configureLogsForChannelResponse_channelName :: Lens.Lens' ConfigureLogsForChannelResponse (Prelude.Maybe Prelude.Text)
configureLogsForChannelResponse_channelName = Lens.lens (\ConfigureLogsForChannelResponse' {channelName} -> channelName) (\s@ConfigureLogsForChannelResponse' {} a -> s {channelName = a} :: ConfigureLogsForChannelResponse)

-- | The types of logs collected.
configureLogsForChannelResponse_logTypes :: Lens.Lens' ConfigureLogsForChannelResponse (Prelude.Maybe [LogType])
configureLogsForChannelResponse_logTypes = Lens.lens (\ConfigureLogsForChannelResponse' {logTypes} -> logTypes) (\s@ConfigureLogsForChannelResponse' {} a -> s {logTypes = a} :: ConfigureLogsForChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
configureLogsForChannelResponse_httpStatus :: Lens.Lens' ConfigureLogsForChannelResponse Prelude.Int
configureLogsForChannelResponse_httpStatus = Lens.lens (\ConfigureLogsForChannelResponse' {httpStatus} -> httpStatus) (\s@ConfigureLogsForChannelResponse' {} a -> s {httpStatus = a} :: ConfigureLogsForChannelResponse)

instance
  Prelude.NFData
    ConfigureLogsForChannelResponse
  where
  rnf ConfigureLogsForChannelResponse' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf logTypes
      `Prelude.seq` Prelude.rnf httpStatus
