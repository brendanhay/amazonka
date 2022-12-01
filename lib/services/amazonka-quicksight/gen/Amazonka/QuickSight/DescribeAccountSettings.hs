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
-- Module      : Amazonka.QuickSight.DescribeAccountSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings that were used when your Amazon QuickSight
-- subscription was first created in this Amazon Web Services account.
module Amazonka.QuickSight.DescribeAccountSettings
  ( -- * Creating a Request
    DescribeAccountSettings (..),
    newDescribeAccountSettings,

    -- * Request Lenses
    describeAccountSettings_awsAccountId,

    -- * Destructuring the Response
    DescribeAccountSettingsResponse (..),
    newDescribeAccountSettingsResponse,

    -- * Response Lenses
    describeAccountSettingsResponse_requestId,
    describeAccountSettingsResponse_accountSettings,
    describeAccountSettingsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountSettings' smart constructor.
data DescribeAccountSettings = DescribeAccountSettings'
  { -- | The ID for the Amazon Web Services account that contains the settings
    -- that you want to list.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeAccountSettings_awsAccountId' - The ID for the Amazon Web Services account that contains the settings
-- that you want to list.
newDescribeAccountSettings ::
  -- | 'awsAccountId'
  Prelude.Text ->
  DescribeAccountSettings
newDescribeAccountSettings pAwsAccountId_ =
  DescribeAccountSettings'
    { awsAccountId =
        pAwsAccountId_
    }

-- | The ID for the Amazon Web Services account that contains the settings
-- that you want to list.
describeAccountSettings_awsAccountId :: Lens.Lens' DescribeAccountSettings Prelude.Text
describeAccountSettings_awsAccountId = Lens.lens (\DescribeAccountSettings' {awsAccountId} -> awsAccountId) (\s@DescribeAccountSettings' {} a -> s {awsAccountId = a} :: DescribeAccountSettings)

instance Core.AWSRequest DescribeAccountSettings where
  type
    AWSResponse DescribeAccountSettings =
      DescribeAccountSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountSettingsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "AccountSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountSettings where
  hashWithSalt _salt DescribeAccountSettings' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData DescribeAccountSettings where
  rnf DescribeAccountSettings' {..} =
    Prelude.rnf awsAccountId

instance Core.ToHeaders DescribeAccountSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAccountSettings where
  toPath DescribeAccountSettings' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS awsAccountId, "/settings"]

instance Core.ToQuery DescribeAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountSettingsResponse' smart constructor.
data DescribeAccountSettingsResponse = DescribeAccountSettingsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight settings for this Amazon Web Services account.
    -- This information includes the edition of Amazon Amazon QuickSight that
    -- you subscribed to (Standard or Enterprise) and the notification email
    -- for the Amazon QuickSight subscription.
    --
    -- In the QuickSight console, the Amazon QuickSight subscription is
    -- sometimes referred to as a QuickSight \"account\" even though it\'s
    -- technically not an account by itself. Instead, it\'s a subscription to
    -- the Amazon QuickSight service for your Amazon Web Services account. The
    -- edition that you subscribe to applies to Amazon QuickSight in every
    -- Amazon Web Services Region where you use it.
    accountSettings :: Prelude.Maybe AccountSettings,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeAccountSettingsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'accountSettings', 'describeAccountSettingsResponse_accountSettings' - The Amazon QuickSight settings for this Amazon Web Services account.
-- This information includes the edition of Amazon Amazon QuickSight that
-- you subscribed to (Standard or Enterprise) and the notification email
-- for the Amazon QuickSight subscription.
--
-- In the QuickSight console, the Amazon QuickSight subscription is
-- sometimes referred to as a QuickSight \"account\" even though it\'s
-- technically not an account by itself. Instead, it\'s a subscription to
-- the Amazon QuickSight service for your Amazon Web Services account. The
-- edition that you subscribe to applies to Amazon QuickSight in every
-- Amazon Web Services Region where you use it.
--
-- 'status', 'describeAccountSettingsResponse_status' - The HTTP status of the request.
newDescribeAccountSettingsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeAccountSettingsResponse
newDescribeAccountSettingsResponse pStatus_ =
  DescribeAccountSettingsResponse'
    { requestId =
        Prelude.Nothing,
      accountSettings = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeAccountSettingsResponse_requestId :: Lens.Lens' DescribeAccountSettingsResponse (Prelude.Maybe Prelude.Text)
describeAccountSettingsResponse_requestId = Lens.lens (\DescribeAccountSettingsResponse' {requestId} -> requestId) (\s@DescribeAccountSettingsResponse' {} a -> s {requestId = a} :: DescribeAccountSettingsResponse)

-- | The Amazon QuickSight settings for this Amazon Web Services account.
-- This information includes the edition of Amazon Amazon QuickSight that
-- you subscribed to (Standard or Enterprise) and the notification email
-- for the Amazon QuickSight subscription.
--
-- In the QuickSight console, the Amazon QuickSight subscription is
-- sometimes referred to as a QuickSight \"account\" even though it\'s
-- technically not an account by itself. Instead, it\'s a subscription to
-- the Amazon QuickSight service for your Amazon Web Services account. The
-- edition that you subscribe to applies to Amazon QuickSight in every
-- Amazon Web Services Region where you use it.
describeAccountSettingsResponse_accountSettings :: Lens.Lens' DescribeAccountSettingsResponse (Prelude.Maybe AccountSettings)
describeAccountSettingsResponse_accountSettings = Lens.lens (\DescribeAccountSettingsResponse' {accountSettings} -> accountSettings) (\s@DescribeAccountSettingsResponse' {} a -> s {accountSettings = a} :: DescribeAccountSettingsResponse)

-- | The HTTP status of the request.
describeAccountSettingsResponse_status :: Lens.Lens' DescribeAccountSettingsResponse Prelude.Int
describeAccountSettingsResponse_status = Lens.lens (\DescribeAccountSettingsResponse' {status} -> status) (\s@DescribeAccountSettingsResponse' {} a -> s {status = a} :: DescribeAccountSettingsResponse)

instance
  Prelude.NFData
    DescribeAccountSettingsResponse
  where
  rnf DescribeAccountSettingsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf accountSettings
      `Prelude.seq` Prelude.rnf status
