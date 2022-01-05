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
-- Module      : Amazonka.SES.UpdateAccountSendingEnabled
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
module Amazonka.SES.UpdateAccountSendingEnabled
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to enable or disable the email sending capabilities
-- for your entire Amazon SES account.
--
-- /See:/ 'newUpdateAccountSendingEnabled' smart constructor.
data UpdateAccountSendingEnabled = UpdateAccountSendingEnabled'
  { -- | Describes whether email sending is enabled or disabled for your Amazon
    -- SES account in the current AWS Region.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Describes whether email sending is enabled or disabled for your Amazon
-- SES account in the current AWS Region.
updateAccountSendingEnabled_enabled :: Lens.Lens' UpdateAccountSendingEnabled (Prelude.Maybe Prelude.Bool)
updateAccountSendingEnabled_enabled = Lens.lens (\UpdateAccountSendingEnabled' {enabled} -> enabled) (\s@UpdateAccountSendingEnabled' {} a -> s {enabled = a} :: UpdateAccountSendingEnabled)

instance Core.AWSRequest UpdateAccountSendingEnabled where
  type
    AWSResponse UpdateAccountSendingEnabled =
      UpdateAccountSendingEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateAccountSendingEnabledResponse'

instance Prelude.Hashable UpdateAccountSendingEnabled where
  hashWithSalt _salt UpdateAccountSendingEnabled' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData UpdateAccountSendingEnabled where
  rnf UpdateAccountSendingEnabled' {..} =
    Prelude.rnf enabled

instance Core.ToHeaders UpdateAccountSendingEnabled where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateAccountSendingEnabled where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateAccountSendingEnabled where
  toQuery UpdateAccountSendingEnabled' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "UpdateAccountSendingEnabled" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Enabled" Core.=: enabled
      ]

-- | /See:/ 'newUpdateAccountSendingEnabledResponse' smart constructor.
data UpdateAccountSendingEnabledResponse = UpdateAccountSendingEnabledResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSendingEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAccountSendingEnabledResponse ::
  UpdateAccountSendingEnabledResponse
newUpdateAccountSendingEnabledResponse =
  UpdateAccountSendingEnabledResponse'

instance
  Prelude.NFData
    UpdateAccountSendingEnabledResponse
  where
  rnf _ = ()
