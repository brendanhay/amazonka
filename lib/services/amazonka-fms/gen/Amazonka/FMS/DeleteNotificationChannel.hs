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
-- Module      : Amazonka.FMS.DeleteNotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Firewall Manager association with the IAM role and the Amazon
-- Simple Notification Service (SNS) topic that is used to record Firewall
-- Manager SNS logs.
module Amazonka.FMS.DeleteNotificationChannel
  ( -- * Creating a Request
    DeleteNotificationChannel (..),
    newDeleteNotificationChannel,

    -- * Destructuring the Response
    DeleteNotificationChannelResponse (..),
    newDeleteNotificationChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNotificationChannel' smart constructor.
data DeleteNotificationChannel = DeleteNotificationChannel'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteNotificationChannelResponse'

instance Prelude.Hashable DeleteNotificationChannel where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteNotificationChannel where
  rnf _ = ()

instance Data.ToHeaders DeleteNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.DeleteNotificationChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteNotificationChannel where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteNotificationChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotificationChannelResponse' smart constructor.
data DeleteNotificationChannelResponse = DeleteNotificationChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
