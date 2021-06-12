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
-- Module      : Network.AWS.SES.UpdateAccountSendingEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending across your entire Amazon SES account
-- in the current AWS Region. You can use this operation in conjunction
-- with Amazon CloudWatch alarms to temporarily pause email sending across
-- your Amazon SES account in a given AWS Region when reputation metrics
-- (such as your bounce or complaint rates) reach certain thresholds.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateAccountSendingEnabled
  ( -- * Creating a Request
    UpdateAccountSendingEnabled (..),
    newUpdateAccountSendingEnabled,

    -- * Request Lenses
    updateAccountSendingEnabled_enabled,

    -- * Destructuring the Response
    UpdateAccountSendingEnabledResponse (..),
    newUpdateAccountSendingEnabledResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to enable or disable the email sending capabilities
-- for your entire Amazon SES account.
--
-- /See:/ 'newUpdateAccountSendingEnabled' smart constructor.
data UpdateAccountSendingEnabled = UpdateAccountSendingEnabled'
  { -- | Describes whether email sending is enabled or disabled for your Amazon
    -- SES account in the current AWS Region.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAccountSendingEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'updateAccountSendingEnabled_enabled' - Describes whether email sending is enabled or disabled for your Amazon
-- SES account in the current AWS Region.
newUpdateAccountSendingEnabled ::
  UpdateAccountSendingEnabled
newUpdateAccountSendingEnabled =
  UpdateAccountSendingEnabled'
    { enabled =
        Core.Nothing
    }

-- | Describes whether email sending is enabled or disabled for your Amazon
-- SES account in the current AWS Region.
updateAccountSendingEnabled_enabled :: Lens.Lens' UpdateAccountSendingEnabled (Core.Maybe Core.Bool)
updateAccountSendingEnabled_enabled = Lens.lens (\UpdateAccountSendingEnabled' {enabled} -> enabled) (\s@UpdateAccountSendingEnabled' {} a -> s {enabled = a} :: UpdateAccountSendingEnabled)

instance Core.AWSRequest UpdateAccountSendingEnabled where
  type
    AWSResponse UpdateAccountSendingEnabled =
      UpdateAccountSendingEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateAccountSendingEnabledResponse'

instance Core.Hashable UpdateAccountSendingEnabled

instance Core.NFData UpdateAccountSendingEnabled

instance Core.ToHeaders UpdateAccountSendingEnabled where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateAccountSendingEnabled where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAccountSendingEnabled where
  toQuery UpdateAccountSendingEnabled' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateAccountSendingEnabled" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Enabled" Core.=: enabled
      ]

-- | /See:/ 'newUpdateAccountSendingEnabledResponse' smart constructor.
data UpdateAccountSendingEnabledResponse = UpdateAccountSendingEnabledResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAccountSendingEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAccountSendingEnabledResponse ::
  UpdateAccountSendingEnabledResponse
newUpdateAccountSendingEnabledResponse =
  UpdateAccountSendingEnabledResponse'

instance
  Core.NFData
    UpdateAccountSendingEnabledResponse
