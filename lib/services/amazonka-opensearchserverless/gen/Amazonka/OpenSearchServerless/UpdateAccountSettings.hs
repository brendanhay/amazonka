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
-- Module      : Amazonka.OpenSearchServerless.UpdateAccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the OpenSearch Serverless settings for the current Amazon Web
-- Services account. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-scaling.html Managing capacity limits for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.UpdateAccountSettings
  ( -- * Creating a Request
    UpdateAccountSettings (..),
    newUpdateAccountSettings,

    -- * Request Lenses
    updateAccountSettings_capacityLimits,

    -- * Destructuring the Response
    UpdateAccountSettingsResponse (..),
    newUpdateAccountSettingsResponse,

    -- * Response Lenses
    updateAccountSettingsResponse_accountSettingsDetail,
    updateAccountSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccountSettings' smart constructor.
data UpdateAccountSettings = UpdateAccountSettings'
  { capacityLimits :: Prelude.Maybe CapacityLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityLimits', 'updateAccountSettings_capacityLimits' - Undocumented member.
newUpdateAccountSettings ::
  UpdateAccountSettings
newUpdateAccountSettings =
  UpdateAccountSettings'
    { capacityLimits =
        Prelude.Nothing
    }

-- | Undocumented member.
updateAccountSettings_capacityLimits :: Lens.Lens' UpdateAccountSettings (Prelude.Maybe CapacityLimits)
updateAccountSettings_capacityLimits = Lens.lens (\UpdateAccountSettings' {capacityLimits} -> capacityLimits) (\s@UpdateAccountSettings' {} a -> s {capacityLimits = a} :: UpdateAccountSettings)

instance Core.AWSRequest UpdateAccountSettings where
  type
    AWSResponse UpdateAccountSettings =
      UpdateAccountSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAccountSettingsResponse'
            Prelude.<$> (x Data..?> "accountSettingsDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAccountSettings where
  hashWithSalt _salt UpdateAccountSettings' {..} =
    _salt `Prelude.hashWithSalt` capacityLimits

instance Prelude.NFData UpdateAccountSettings where
  rnf UpdateAccountSettings' {..} =
    Prelude.rnf capacityLimits

instance Data.ToHeaders UpdateAccountSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.UpdateAccountSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAccountSettings where
  toJSON UpdateAccountSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capacityLimits" Data..=)
              Prelude.<$> capacityLimits
          ]
      )

instance Data.ToPath UpdateAccountSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccountSettingsResponse' smart constructor.
data UpdateAccountSettingsResponse = UpdateAccountSettingsResponse'
  { -- | OpenSearch Serverless-related settings for the current Amazon Web
    -- Services account.
    accountSettingsDetail :: Prelude.Maybe AccountSettingsDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountSettingsDetail', 'updateAccountSettingsResponse_accountSettingsDetail' - OpenSearch Serverless-related settings for the current Amazon Web
-- Services account.
--
-- 'httpStatus', 'updateAccountSettingsResponse_httpStatus' - The response's http status code.
newUpdateAccountSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAccountSettingsResponse
newUpdateAccountSettingsResponse pHttpStatus_ =
  UpdateAccountSettingsResponse'
    { accountSettingsDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | OpenSearch Serverless-related settings for the current Amazon Web
-- Services account.
updateAccountSettingsResponse_accountSettingsDetail :: Lens.Lens' UpdateAccountSettingsResponse (Prelude.Maybe AccountSettingsDetail)
updateAccountSettingsResponse_accountSettingsDetail = Lens.lens (\UpdateAccountSettingsResponse' {accountSettingsDetail} -> accountSettingsDetail) (\s@UpdateAccountSettingsResponse' {} a -> s {accountSettingsDetail = a} :: UpdateAccountSettingsResponse)

-- | The response's http status code.
updateAccountSettingsResponse_httpStatus :: Lens.Lens' UpdateAccountSettingsResponse Prelude.Int
updateAccountSettingsResponse_httpStatus = Lens.lens (\UpdateAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateAccountSettingsResponse' {} a -> s {httpStatus = a} :: UpdateAccountSettingsResponse)

instance Prelude.NFData UpdateAccountSettingsResponse where
  rnf UpdateAccountSettingsResponse' {..} =
    Prelude.rnf accountSettingsDetail
      `Prelude.seq` Prelude.rnf httpStatus
