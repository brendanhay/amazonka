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
-- Module      : Amazonka.QuickSight.UpdatePublicSharingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @UpdatePublicSharingSettings@ operation to turn on or turn off
-- the public sharing settings of an Amazon QuickSight dashboard.
--
-- To use this operation, turn on session capacity pricing for your Amazon
-- QuickSight account.
--
-- Before you can turn on public sharing on your account, make sure to give
-- public sharing permissions to an administrative user in the Identity and
-- Access Management (IAM) console. For more information on using IAM with
-- Amazon QuickSight, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/security_iam_service-with-iam.html Using Amazon QuickSight with IAM>
-- in the /Amazon QuickSight User Guide/.
module Amazonka.QuickSight.UpdatePublicSharingSettings
  ( -- * Creating a Request
    UpdatePublicSharingSettings (..),
    newUpdatePublicSharingSettings,

    -- * Request Lenses
    updatePublicSharingSettings_publicSharingEnabled,
    updatePublicSharingSettings_awsAccountId,

    -- * Destructuring the Response
    UpdatePublicSharingSettingsResponse (..),
    newUpdatePublicSharingSettingsResponse,

    -- * Response Lenses
    updatePublicSharingSettingsResponse_requestId,
    updatePublicSharingSettingsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePublicSharingSettings' smart constructor.
data UpdatePublicSharingSettings = UpdatePublicSharingSettings'
  { -- | A Boolean value that indicates whether public sharing is turned on for
    -- an Amazon QuickSight account.
    publicSharingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account ID associated with your Amazon
    -- QuickSight subscription.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePublicSharingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicSharingEnabled', 'updatePublicSharingSettings_publicSharingEnabled' - A Boolean value that indicates whether public sharing is turned on for
-- an Amazon QuickSight account.
--
-- 'awsAccountId', 'updatePublicSharingSettings_awsAccountId' - The Amazon Web Services account ID associated with your Amazon
-- QuickSight subscription.
newUpdatePublicSharingSettings ::
  -- | 'awsAccountId'
  Prelude.Text ->
  UpdatePublicSharingSettings
newUpdatePublicSharingSettings pAwsAccountId_ =
  UpdatePublicSharingSettings'
    { publicSharingEnabled =
        Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | A Boolean value that indicates whether public sharing is turned on for
-- an Amazon QuickSight account.
updatePublicSharingSettings_publicSharingEnabled :: Lens.Lens' UpdatePublicSharingSettings (Prelude.Maybe Prelude.Bool)
updatePublicSharingSettings_publicSharingEnabled = Lens.lens (\UpdatePublicSharingSettings' {publicSharingEnabled} -> publicSharingEnabled) (\s@UpdatePublicSharingSettings' {} a -> s {publicSharingEnabled = a} :: UpdatePublicSharingSettings)

-- | The Amazon Web Services account ID associated with your Amazon
-- QuickSight subscription.
updatePublicSharingSettings_awsAccountId :: Lens.Lens' UpdatePublicSharingSettings Prelude.Text
updatePublicSharingSettings_awsAccountId = Lens.lens (\UpdatePublicSharingSettings' {awsAccountId} -> awsAccountId) (\s@UpdatePublicSharingSettings' {} a -> s {awsAccountId = a} :: UpdatePublicSharingSettings)

instance Core.AWSRequest UpdatePublicSharingSettings where
  type
    AWSResponse UpdatePublicSharingSettings =
      UpdatePublicSharingSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePublicSharingSettingsResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePublicSharingSettings where
  hashWithSalt _salt UpdatePublicSharingSettings' {..} =
    _salt
      `Prelude.hashWithSalt` publicSharingEnabled
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData UpdatePublicSharingSettings where
  rnf UpdatePublicSharingSettings' {..} =
    Prelude.rnf publicSharingEnabled
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders UpdatePublicSharingSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePublicSharingSettings where
  toJSON UpdatePublicSharingSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PublicSharingEnabled" Data..=)
              Prelude.<$> publicSharingEnabled
          ]
      )

instance Data.ToPath UpdatePublicSharingSettings where
  toPath UpdatePublicSharingSettings' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/public-sharing-settings"
      ]

instance Data.ToQuery UpdatePublicSharingSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePublicSharingSettingsResponse' smart constructor.
data UpdatePublicSharingSettingsResponse = UpdatePublicSharingSettingsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePublicSharingSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updatePublicSharingSettingsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updatePublicSharingSettingsResponse_status' - The HTTP status of the request.
newUpdatePublicSharingSettingsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdatePublicSharingSettingsResponse
newUpdatePublicSharingSettingsResponse pStatus_ =
  UpdatePublicSharingSettingsResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updatePublicSharingSettingsResponse_requestId :: Lens.Lens' UpdatePublicSharingSettingsResponse (Prelude.Maybe Prelude.Text)
updatePublicSharingSettingsResponse_requestId = Lens.lens (\UpdatePublicSharingSettingsResponse' {requestId} -> requestId) (\s@UpdatePublicSharingSettingsResponse' {} a -> s {requestId = a} :: UpdatePublicSharingSettingsResponse)

-- | The HTTP status of the request.
updatePublicSharingSettingsResponse_status :: Lens.Lens' UpdatePublicSharingSettingsResponse Prelude.Int
updatePublicSharingSettingsResponse_status = Lens.lens (\UpdatePublicSharingSettingsResponse' {status} -> status) (\s@UpdatePublicSharingSettingsResponse' {} a -> s {status = a} :: UpdatePublicSharingSettingsResponse)

instance
  Prelude.NFData
    UpdatePublicSharingSettingsResponse
  where
  rnf UpdatePublicSharingSettingsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
