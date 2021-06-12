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
-- Module      : Network.AWS.FMS.DeleteNotificationChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Firewall Manager association with the IAM role and the
-- Amazon Simple Notification Service (SNS) topic that is used to record
-- AWS Firewall Manager SNS logs.
module Network.AWS.FMS.DeleteNotificationChannel
  ( -- * Creating a Request
    DeleteNotificationChannel (..),
    newDeleteNotificationChannel,

    -- * Destructuring the Response
    DeleteNotificationChannelResponse (..),
    newDeleteNotificationChannelResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNotificationChannel' smart constructor.
data DeleteNotificationChannel = DeleteNotificationChannel'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationChannel ::
  DeleteNotificationChannel
newDeleteNotificationChannel =
  DeleteNotificationChannel'

instance Core.AWSRequest DeleteNotificationChannel where
  type
    AWSResponse DeleteNotificationChannel =
      DeleteNotificationChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteNotificationChannelResponse'

instance Core.Hashable DeleteNotificationChannel

instance Core.NFData DeleteNotificationChannel

instance Core.ToHeaders DeleteNotificationChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.DeleteNotificationChannel" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteNotificationChannel where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DeleteNotificationChannel where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNotificationChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteNotificationChannelResponse' smart constructor.
data DeleteNotificationChannelResponse = DeleteNotificationChannelResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationChannelResponse ::
  DeleteNotificationChannelResponse
newDeleteNotificationChannelResponse =
  DeleteNotificationChannelResponse'

instance
  Core.NFData
    DeleteNotificationChannelResponse
