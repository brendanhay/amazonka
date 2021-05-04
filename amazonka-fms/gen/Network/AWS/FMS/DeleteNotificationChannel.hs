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

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNotificationChannel' smart constructor.
data DeleteNotificationChannel = DeleteNotificationChannel'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationChannel ::
  DeleteNotificationChannel
newDeleteNotificationChannel =
  DeleteNotificationChannel'

instance Prelude.AWSRequest DeleteNotificationChannel where
  type
    Rs DeleteNotificationChannel =
      DeleteNotificationChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteNotificationChannelResponse'

instance Prelude.Hashable DeleteNotificationChannel

instance Prelude.NFData DeleteNotificationChannel

instance Prelude.ToHeaders DeleteNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.DeleteNotificationChannel" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteNotificationChannel where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DeleteNotificationChannel where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotificationChannelResponse' smart constructor.
data DeleteNotificationChannelResponse = DeleteNotificationChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationChannelResponse ::
  DeleteNotificationChannelResponse
newDeleteNotificationChannelResponse =
  DeleteNotificationChannelResponse'

instance
  Prelude.NFData
    DeleteNotificationChannelResponse
