{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.UpdateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a channel.
module Network.AWS.IoTAnalytics.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_retentionPeriod,
    updateChannel_channelStorage,
    updateChannel_channelName,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | How long, in days, message data is kept for the channel. The retention
    -- period cannot be updated if the channel\'s S3 storage is
    -- customer-managed.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You cannot change this storage option after the
    -- channel is created.
    channelStorage :: Prelude.Maybe ChannelStorage,
    -- | The name of the channel to be updated.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriod', 'updateChannel_retentionPeriod' - How long, in days, message data is kept for the channel. The retention
-- period cannot be updated if the channel\'s S3 storage is
-- customer-managed.
--
-- 'channelStorage', 'updateChannel_channelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You cannot change this storage option after the
-- channel is created.
--
-- 'channelName', 'updateChannel_channelName' - The name of the channel to be updated.
newUpdateChannel ::
  -- | 'channelName'
  Prelude.Text ->
  UpdateChannel
newUpdateChannel pChannelName_ =
  UpdateChannel'
    { retentionPeriod = Prelude.Nothing,
      channelStorage = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | How long, in days, message data is kept for the channel. The retention
-- period cannot be updated if the channel\'s S3 storage is
-- customer-managed.
updateChannel_retentionPeriod :: Lens.Lens' UpdateChannel (Prelude.Maybe RetentionPeriod)
updateChannel_retentionPeriod = Lens.lens (\UpdateChannel' {retentionPeriod} -> retentionPeriod) (\s@UpdateChannel' {} a -> s {retentionPeriod = a} :: UpdateChannel)

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You cannot change this storage option after the
-- channel is created.
updateChannel_channelStorage :: Lens.Lens' UpdateChannel (Prelude.Maybe ChannelStorage)
updateChannel_channelStorage = Lens.lens (\UpdateChannel' {channelStorage} -> channelStorage) (\s@UpdateChannel' {} a -> s {channelStorage = a} :: UpdateChannel)

-- | The name of the channel to be updated.
updateChannel_channelName :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_channelName = Lens.lens (\UpdateChannel' {channelName} -> channelName) (\s@UpdateChannel' {} a -> s {channelName = a} :: UpdateChannel)

instance Prelude.AWSRequest UpdateChannel where
  type Rs UpdateChannel = UpdateChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateChannelResponse'

instance Prelude.Hashable UpdateChannel

instance Prelude.NFData UpdateChannel

instance Prelude.ToHeaders UpdateChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("retentionPeriod" Prelude..=)
              Prelude.<$> retentionPeriod,
            ("channelStorage" Prelude..=)
              Prelude.<$> channelStorage
          ]
      )

instance Prelude.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Prelude.mconcat
      ["/channels/", Prelude.toBS channelName]

instance Prelude.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateChannelResponse ::
  UpdateChannelResponse
newUpdateChannelResponse = UpdateChannelResponse'

instance Prelude.NFData UpdateChannelResponse
