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
-- Module      : Amazonka.IoTSiteWise.UpdatePortal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an IoT SiteWise Monitor portal.
module Amazonka.IoTSiteWise.UpdatePortal
  ( -- * Creating a Request
    UpdatePortal (..),
    newUpdatePortal,

    -- * Request Lenses
    updatePortal_alarms,
    updatePortal_clientToken,
    updatePortal_notificationSenderEmail,
    updatePortal_portalDescription,
    updatePortal_portalLogoImage,
    updatePortal_portalId,
    updatePortal_portalName,
    updatePortal_portalContactEmail,
    updatePortal_roleArn,

    -- * Destructuring the Response
    UpdatePortalResponse (..),
    newUpdatePortalResponse,

    -- * Response Lenses
    updatePortalResponse_httpStatus,
    updatePortalResponse_portalStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePortal' smart constructor.
data UpdatePortal = UpdatePortal'
  { -- | Contains the configuration information of an alarm created in an IoT
    -- SiteWise Monitor portal. You can use the alarm to monitor an asset
    -- property and get notified when the asset property value is outside a
    -- specified range. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/appguide/monitor-alarms.html Monitoring with alarms>
    -- in the /IoT SiteWise Application Guide/.
    alarms :: Prelude.Maybe Alarms,
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The email address that sends alarm notifications.
    notificationSenderEmail :: Prelude.Maybe Prelude.Text,
    -- | A new description for the portal.
    portalDescription :: Prelude.Maybe Prelude.Text,
    portalLogoImage :: Prelude.Maybe Image,
    -- | The ID of the portal to update.
    portalId :: Prelude.Text,
    -- | A new friendly name for the portal.
    portalName :: Prelude.Text,
    -- | The Amazon Web Services administrator\'s contact email address.
    portalContactEmail :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of a service role that allows the portal\'s users to access your IoT
    -- SiteWise resources on your behalf. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
    -- in the /IoT SiteWise User Guide/.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePortal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarms', 'updatePortal_alarms' - Contains the configuration information of an alarm created in an IoT
-- SiteWise Monitor portal. You can use the alarm to monitor an asset
-- property and get notified when the asset property value is outside a
-- specified range. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/appguide/monitor-alarms.html Monitoring with alarms>
-- in the /IoT SiteWise Application Guide/.
--
-- 'clientToken', 'updatePortal_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'notificationSenderEmail', 'updatePortal_notificationSenderEmail' - The email address that sends alarm notifications.
--
-- 'portalDescription', 'updatePortal_portalDescription' - A new description for the portal.
--
-- 'portalLogoImage', 'updatePortal_portalLogoImage' - Undocumented member.
--
-- 'portalId', 'updatePortal_portalId' - The ID of the portal to update.
--
-- 'portalName', 'updatePortal_portalName' - A new friendly name for the portal.
--
-- 'portalContactEmail', 'updatePortal_portalContactEmail' - The Amazon Web Services administrator\'s contact email address.
--
-- 'roleArn', 'updatePortal_roleArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of a service role that allows the portal\'s users to access your IoT
-- SiteWise resources on your behalf. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
-- in the /IoT SiteWise User Guide/.
newUpdatePortal ::
  -- | 'portalId'
  Prelude.Text ->
  -- | 'portalName'
  Prelude.Text ->
  -- | 'portalContactEmail'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  UpdatePortal
newUpdatePortal
  pPortalId_
  pPortalName_
  pPortalContactEmail_
  pRoleArn_ =
    UpdatePortal'
      { alarms = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        notificationSenderEmail = Prelude.Nothing,
        portalDescription = Prelude.Nothing,
        portalLogoImage = Prelude.Nothing,
        portalId = pPortalId_,
        portalName = pPortalName_,
        portalContactEmail = pPortalContactEmail_,
        roleArn = pRoleArn_
      }

-- | Contains the configuration information of an alarm created in an IoT
-- SiteWise Monitor portal. You can use the alarm to monitor an asset
-- property and get notified when the asset property value is outside a
-- specified range. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/appguide/monitor-alarms.html Monitoring with alarms>
-- in the /IoT SiteWise Application Guide/.
updatePortal_alarms :: Lens.Lens' UpdatePortal (Prelude.Maybe Alarms)
updatePortal_alarms = Lens.lens (\UpdatePortal' {alarms} -> alarms) (\s@UpdatePortal' {} a -> s {alarms = a} :: UpdatePortal)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updatePortal_clientToken :: Lens.Lens' UpdatePortal (Prelude.Maybe Prelude.Text)
updatePortal_clientToken = Lens.lens (\UpdatePortal' {clientToken} -> clientToken) (\s@UpdatePortal' {} a -> s {clientToken = a} :: UpdatePortal)

-- | The email address that sends alarm notifications.
updatePortal_notificationSenderEmail :: Lens.Lens' UpdatePortal (Prelude.Maybe Prelude.Text)
updatePortal_notificationSenderEmail = Lens.lens (\UpdatePortal' {notificationSenderEmail} -> notificationSenderEmail) (\s@UpdatePortal' {} a -> s {notificationSenderEmail = a} :: UpdatePortal)

-- | A new description for the portal.
updatePortal_portalDescription :: Lens.Lens' UpdatePortal (Prelude.Maybe Prelude.Text)
updatePortal_portalDescription = Lens.lens (\UpdatePortal' {portalDescription} -> portalDescription) (\s@UpdatePortal' {} a -> s {portalDescription = a} :: UpdatePortal)

-- | Undocumented member.
updatePortal_portalLogoImage :: Lens.Lens' UpdatePortal (Prelude.Maybe Image)
updatePortal_portalLogoImage = Lens.lens (\UpdatePortal' {portalLogoImage} -> portalLogoImage) (\s@UpdatePortal' {} a -> s {portalLogoImage = a} :: UpdatePortal)

-- | The ID of the portal to update.
updatePortal_portalId :: Lens.Lens' UpdatePortal Prelude.Text
updatePortal_portalId = Lens.lens (\UpdatePortal' {portalId} -> portalId) (\s@UpdatePortal' {} a -> s {portalId = a} :: UpdatePortal)

-- | A new friendly name for the portal.
updatePortal_portalName :: Lens.Lens' UpdatePortal Prelude.Text
updatePortal_portalName = Lens.lens (\UpdatePortal' {portalName} -> portalName) (\s@UpdatePortal' {} a -> s {portalName = a} :: UpdatePortal)

-- | The Amazon Web Services administrator\'s contact email address.
updatePortal_portalContactEmail :: Lens.Lens' UpdatePortal Prelude.Text
updatePortal_portalContactEmail = Lens.lens (\UpdatePortal' {portalContactEmail} -> portalContactEmail) (\s@UpdatePortal' {} a -> s {portalContactEmail = a} :: UpdatePortal)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of a service role that allows the portal\'s users to access your IoT
-- SiteWise resources on your behalf. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
-- in the /IoT SiteWise User Guide/.
updatePortal_roleArn :: Lens.Lens' UpdatePortal Prelude.Text
updatePortal_roleArn = Lens.lens (\UpdatePortal' {roleArn} -> roleArn) (\s@UpdatePortal' {} a -> s {roleArn = a} :: UpdatePortal)

instance Core.AWSRequest UpdatePortal where
  type AWSResponse UpdatePortal = UpdatePortalResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePortalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalStatus")
      )

instance Prelude.Hashable UpdatePortal where
  hashWithSalt _salt UpdatePortal' {..} =
    _salt `Prelude.hashWithSalt` alarms
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` notificationSenderEmail
      `Prelude.hashWithSalt` portalDescription
      `Prelude.hashWithSalt` portalLogoImage
      `Prelude.hashWithSalt` portalId
      `Prelude.hashWithSalt` portalName
      `Prelude.hashWithSalt` portalContactEmail
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData UpdatePortal where
  rnf UpdatePortal' {..} =
    Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf notificationSenderEmail
      `Prelude.seq` Prelude.rnf portalDescription
      `Prelude.seq` Prelude.rnf portalLogoImage
      `Prelude.seq` Prelude.rnf portalId
      `Prelude.seq` Prelude.rnf portalName
      `Prelude.seq` Prelude.rnf portalContactEmail
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders UpdatePortal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePortal where
  toJSON UpdatePortal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alarms" Data..=) Prelude.<$> alarms,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("notificationSenderEmail" Data..=)
              Prelude.<$> notificationSenderEmail,
            ("portalDescription" Data..=)
              Prelude.<$> portalDescription,
            ("portalLogoImage" Data..=)
              Prelude.<$> portalLogoImage,
            Prelude.Just ("portalName" Data..= portalName),
            Prelude.Just
              ("portalContactEmail" Data..= portalContactEmail),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath UpdatePortal where
  toPath UpdatePortal' {..} =
    Prelude.mconcat ["/portals/", Data.toBS portalId]

instance Data.ToQuery UpdatePortal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePortalResponse' smart constructor.
data UpdatePortalResponse = UpdatePortalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the portal, which contains a state (@UPDATING@ after
    -- successfully calling this operation) and any error message.
    portalStatus :: PortalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePortalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePortalResponse_httpStatus' - The response's http status code.
--
-- 'portalStatus', 'updatePortalResponse_portalStatus' - The status of the portal, which contains a state (@UPDATING@ after
-- successfully calling this operation) and any error message.
newUpdatePortalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalStatus'
  PortalStatus ->
  UpdatePortalResponse
newUpdatePortalResponse pHttpStatus_ pPortalStatus_ =
  UpdatePortalResponse'
    { httpStatus = pHttpStatus_,
      portalStatus = pPortalStatus_
    }

-- | The response's http status code.
updatePortalResponse_httpStatus :: Lens.Lens' UpdatePortalResponse Prelude.Int
updatePortalResponse_httpStatus = Lens.lens (\UpdatePortalResponse' {httpStatus} -> httpStatus) (\s@UpdatePortalResponse' {} a -> s {httpStatus = a} :: UpdatePortalResponse)

-- | The status of the portal, which contains a state (@UPDATING@ after
-- successfully calling this operation) and any error message.
updatePortalResponse_portalStatus :: Lens.Lens' UpdatePortalResponse PortalStatus
updatePortalResponse_portalStatus = Lens.lens (\UpdatePortalResponse' {portalStatus} -> portalStatus) (\s@UpdatePortalResponse' {} a -> s {portalStatus = a} :: UpdatePortalResponse)

instance Prelude.NFData UpdatePortalResponse where
  rnf UpdatePortalResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf portalStatus
