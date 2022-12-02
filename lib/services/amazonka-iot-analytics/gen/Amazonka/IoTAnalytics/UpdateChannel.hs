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
-- Module      : Amazonka.IoTAnalytics.UpdateChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to update the settings of a channel.
module Amazonka.IoTAnalytics.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_channelStorage,
    updateChannel_retentionPeriod,
    updateChannel_channelName,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You can\'t change this storage option after the
    -- channel is created.
    channelStorage :: Prelude.Maybe ChannelStorage,
    -- | How long, in days, message data is kept for the channel. The retention
    -- period can\'t be updated if the channel\'s Amazon S3 storage is
    -- customer-managed.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The name of the channel to be updated.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelStorage', 'updateChannel_channelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
--
-- 'retentionPeriod', 'updateChannel_retentionPeriod' - How long, in days, message data is kept for the channel. The retention
-- period can\'t be updated if the channel\'s Amazon S3 storage is
-- customer-managed.
--
-- 'channelName', 'updateChannel_channelName' - The name of the channel to be updated.
newUpdateChannel ::
  -- | 'channelName'
  Prelude.Text ->
  UpdateChannel
newUpdateChannel pChannelName_ =
  UpdateChannel'
    { channelStorage = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
updateChannel_channelStorage :: Lens.Lens' UpdateChannel (Prelude.Maybe ChannelStorage)
updateChannel_channelStorage = Lens.lens (\UpdateChannel' {channelStorage} -> channelStorage) (\s@UpdateChannel' {} a -> s {channelStorage = a} :: UpdateChannel)

-- | How long, in days, message data is kept for the channel. The retention
-- period can\'t be updated if the channel\'s Amazon S3 storage is
-- customer-managed.
updateChannel_retentionPeriod :: Lens.Lens' UpdateChannel (Prelude.Maybe RetentionPeriod)
updateChannel_retentionPeriod = Lens.lens (\UpdateChannel' {retentionPeriod} -> retentionPeriod) (\s@UpdateChannel' {} a -> s {retentionPeriod = a} :: UpdateChannel)

-- | The name of the channel to be updated.
updateChannel_channelName :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_channelName = Lens.lens (\UpdateChannel' {channelName} -> channelName) (\s@UpdateChannel' {} a -> s {channelName = a} :: UpdateChannel)

instance Core.AWSRequest UpdateChannel where
  type
    AWSResponse UpdateChannel =
      UpdateChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateChannelResponse'

instance Prelude.Hashable UpdateChannel where
  hashWithSalt _salt UpdateChannel' {..} =
    _salt `Prelude.hashWithSalt` channelStorage
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData UpdateChannel where
  rnf UpdateChannel' {..} =
    Prelude.rnf channelStorage
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders UpdateChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channelStorage" Data..=)
              Prelude.<$> channelStorage,
            ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod
          ]
      )

instance Data.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelName]

instance Data.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateChannelResponse ::
  UpdateChannelResponse
newUpdateChannelResponse = UpdateChannelResponse'

instance Prelude.NFData UpdateChannelResponse where
  rnf _ = ()
