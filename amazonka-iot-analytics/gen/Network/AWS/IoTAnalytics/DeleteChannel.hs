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
-- Module      : Network.AWS.IoTAnalytics.DeleteChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified channel.
module Network.AWS.IoTAnalytics.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_channelName,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | The name of the channel to delete.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'deleteChannel_channelName' - The name of the channel to delete.
newDeleteChannel ::
  -- | 'channelName'
  Prelude.Text ->
  DeleteChannel
newDeleteChannel pChannelName_ =
  DeleteChannel' {channelName = pChannelName_}

-- | The name of the channel to delete.
deleteChannel_channelName :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_channelName = Lens.lens (\DeleteChannel' {channelName} -> channelName) (\s@DeleteChannel' {} a -> s {channelName = a} :: DeleteChannel)

instance Prelude.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteChannelResponse'

instance Prelude.Hashable DeleteChannel

instance Prelude.NFData DeleteChannel

instance Prelude.ToHeaders DeleteChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Prelude.mconcat
      ["/channels/", Prelude.toBS channelName]

instance Prelude.ToQuery DeleteChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteChannelResponse ::
  DeleteChannelResponse
newDeleteChannelResponse = DeleteChannelResponse'

instance Prelude.NFData DeleteChannelResponse
