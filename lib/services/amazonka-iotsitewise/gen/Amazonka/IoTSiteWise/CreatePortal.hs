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
-- Module      : Amazonka.IoTSiteWise.CreatePortal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a portal, which can contain projects and dashboards. IoT
-- SiteWise Monitor uses IAM Identity Center or IAM to authenticate portal
-- users and manage user permissions.
--
-- Before you can sign in to a new portal, you must add at least one
-- identity to that portal. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/administer-portals.html#portal-change-admins Adding or removing portal administrators>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.CreatePortal
  ( -- * Creating a Request
    CreatePortal (..),
    newCreatePortal,

    -- * Request Lenses
    createPortal_tags,
    createPortal_alarms,
    createPortal_clientToken,
    createPortal_portalDescription,
    createPortal_portalAuthMode,
    createPortal_portalLogoImageFile,
    createPortal_notificationSenderEmail,
    createPortal_portalName,
    createPortal_portalContactEmail,
    createPortal_roleArn,

    -- * Destructuring the Response
    CreatePortalResponse (..),
    newCreatePortalResponse,

    -- * Response Lenses
    createPortalResponse_httpStatus,
    createPortalResponse_portalId,
    createPortalResponse_portalArn,
    createPortalResponse_portalStartUrl,
    createPortalResponse_portalStatus,
    createPortalResponse_ssoApplicationId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePortal' smart constructor.
data CreatePortal = CreatePortal'
  { -- | A list of key-value pairs that contain metadata for the portal. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
    -- in the /IoT SiteWise User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains the configuration information of an alarm created in an IoT
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
    -- | A description for the portal.
    portalDescription :: Prelude.Maybe Prelude.Text,
    -- | The service to use to authenticate users to the portal. Choose from the
    -- following options:
    --
    -- -   @SSO@ – The portal uses IAM Identity Center (successor to Single
    --     Sign-On) to authenticate users and manage user permissions. Before
    --     you can create a portal that uses IAM Identity Center, you must
    --     enable IAM Identity Center. For more information, see
    --     <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-get-started.html#mon-gs-sso Enabling IAM Identity Center>
    --     in the /IoT SiteWise User Guide/. This option is only available in
    --     Amazon Web Services Regions other than the China Regions.
    --
    -- -   @IAM@ – The portal uses Identity and Access Management to
    --     authenticate users and manage user permissions.
    --
    -- You can\'t change this value after you create a portal.
    --
    -- Default: @SSO@
    portalAuthMode :: Prelude.Maybe AuthMode,
    -- | A logo image to display in the portal. Upload a square, high-resolution
    -- image. The image is displayed on a dark background.
    portalLogoImageFile :: Prelude.Maybe ImageFile,
    -- | The email address that sends alarm notifications.
    --
    -- If you use the
    -- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html IoT Events managed Lambda function>
    -- to manage your emails, you must
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html verify the sender email address in Amazon SES>.
    notificationSenderEmail :: Prelude.Maybe Prelude.Text,
    -- | A friendly name for the portal.
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
-- Create a value of 'CreatePortal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPortal_tags' - A list of key-value pairs that contain metadata for the portal. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
--
-- 'alarms', 'createPortal_alarms' - Contains the configuration information of an alarm created in an IoT
-- SiteWise Monitor portal. You can use the alarm to monitor an asset
-- property and get notified when the asset property value is outside a
-- specified range. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/appguide/monitor-alarms.html Monitoring with alarms>
-- in the /IoT SiteWise Application Guide/.
--
-- 'clientToken', 'createPortal_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'portalDescription', 'createPortal_portalDescription' - A description for the portal.
--
-- 'portalAuthMode', 'createPortal_portalAuthMode' - The service to use to authenticate users to the portal. Choose from the
-- following options:
--
-- -   @SSO@ – The portal uses IAM Identity Center (successor to Single
--     Sign-On) to authenticate users and manage user permissions. Before
--     you can create a portal that uses IAM Identity Center, you must
--     enable IAM Identity Center. For more information, see
--     <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-get-started.html#mon-gs-sso Enabling IAM Identity Center>
--     in the /IoT SiteWise User Guide/. This option is only available in
--     Amazon Web Services Regions other than the China Regions.
--
-- -   @IAM@ – The portal uses Identity and Access Management to
--     authenticate users and manage user permissions.
--
-- You can\'t change this value after you create a portal.
--
-- Default: @SSO@
--
-- 'portalLogoImageFile', 'createPortal_portalLogoImageFile' - A logo image to display in the portal. Upload a square, high-resolution
-- image. The image is displayed on a dark background.
--
-- 'notificationSenderEmail', 'createPortal_notificationSenderEmail' - The email address that sends alarm notifications.
--
-- If you use the
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html IoT Events managed Lambda function>
-- to manage your emails, you must
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html verify the sender email address in Amazon SES>.
--
-- 'portalName', 'createPortal_portalName' - A friendly name for the portal.
--
-- 'portalContactEmail', 'createPortal_portalContactEmail' - The Amazon Web Services administrator\'s contact email address.
--
-- 'roleArn', 'createPortal_roleArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of a service role that allows the portal\'s users to access your IoT
-- SiteWise resources on your behalf. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
-- in the /IoT SiteWise User Guide/.
newCreatePortal ::
  -- | 'portalName'
  Prelude.Text ->
  -- | 'portalContactEmail'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreatePortal
newCreatePortal
  pPortalName_
  pPortalContactEmail_
  pRoleArn_ =
    CreatePortal'
      { tags = Prelude.Nothing,
        alarms = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        portalDescription = Prelude.Nothing,
        portalAuthMode = Prelude.Nothing,
        portalLogoImageFile = Prelude.Nothing,
        notificationSenderEmail = Prelude.Nothing,
        portalName = pPortalName_,
        portalContactEmail = pPortalContactEmail_,
        roleArn = pRoleArn_
      }

-- | A list of key-value pairs that contain metadata for the portal. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
createPortal_tags :: Lens.Lens' CreatePortal (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPortal_tags = Lens.lens (\CreatePortal' {tags} -> tags) (\s@CreatePortal' {} a -> s {tags = a} :: CreatePortal) Prelude.. Lens.mapping Lens.coerced

-- | Contains the configuration information of an alarm created in an IoT
-- SiteWise Monitor portal. You can use the alarm to monitor an asset
-- property and get notified when the asset property value is outside a
-- specified range. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/appguide/monitor-alarms.html Monitoring with alarms>
-- in the /IoT SiteWise Application Guide/.
createPortal_alarms :: Lens.Lens' CreatePortal (Prelude.Maybe Alarms)
createPortal_alarms = Lens.lens (\CreatePortal' {alarms} -> alarms) (\s@CreatePortal' {} a -> s {alarms = a} :: CreatePortal)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createPortal_clientToken :: Lens.Lens' CreatePortal (Prelude.Maybe Prelude.Text)
createPortal_clientToken = Lens.lens (\CreatePortal' {clientToken} -> clientToken) (\s@CreatePortal' {} a -> s {clientToken = a} :: CreatePortal)

-- | A description for the portal.
createPortal_portalDescription :: Lens.Lens' CreatePortal (Prelude.Maybe Prelude.Text)
createPortal_portalDescription = Lens.lens (\CreatePortal' {portalDescription} -> portalDescription) (\s@CreatePortal' {} a -> s {portalDescription = a} :: CreatePortal)

-- | The service to use to authenticate users to the portal. Choose from the
-- following options:
--
-- -   @SSO@ – The portal uses IAM Identity Center (successor to Single
--     Sign-On) to authenticate users and manage user permissions. Before
--     you can create a portal that uses IAM Identity Center, you must
--     enable IAM Identity Center. For more information, see
--     <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-get-started.html#mon-gs-sso Enabling IAM Identity Center>
--     in the /IoT SiteWise User Guide/. This option is only available in
--     Amazon Web Services Regions other than the China Regions.
--
-- -   @IAM@ – The portal uses Identity and Access Management to
--     authenticate users and manage user permissions.
--
-- You can\'t change this value after you create a portal.
--
-- Default: @SSO@
createPortal_portalAuthMode :: Lens.Lens' CreatePortal (Prelude.Maybe AuthMode)
createPortal_portalAuthMode = Lens.lens (\CreatePortal' {portalAuthMode} -> portalAuthMode) (\s@CreatePortal' {} a -> s {portalAuthMode = a} :: CreatePortal)

-- | A logo image to display in the portal. Upload a square, high-resolution
-- image. The image is displayed on a dark background.
createPortal_portalLogoImageFile :: Lens.Lens' CreatePortal (Prelude.Maybe ImageFile)
createPortal_portalLogoImageFile = Lens.lens (\CreatePortal' {portalLogoImageFile} -> portalLogoImageFile) (\s@CreatePortal' {} a -> s {portalLogoImageFile = a} :: CreatePortal)

-- | The email address that sends alarm notifications.
--
-- If you use the
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html IoT Events managed Lambda function>
-- to manage your emails, you must
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html verify the sender email address in Amazon SES>.
createPortal_notificationSenderEmail :: Lens.Lens' CreatePortal (Prelude.Maybe Prelude.Text)
createPortal_notificationSenderEmail = Lens.lens (\CreatePortal' {notificationSenderEmail} -> notificationSenderEmail) (\s@CreatePortal' {} a -> s {notificationSenderEmail = a} :: CreatePortal)

-- | A friendly name for the portal.
createPortal_portalName :: Lens.Lens' CreatePortal Prelude.Text
createPortal_portalName = Lens.lens (\CreatePortal' {portalName} -> portalName) (\s@CreatePortal' {} a -> s {portalName = a} :: CreatePortal)

-- | The Amazon Web Services administrator\'s contact email address.
createPortal_portalContactEmail :: Lens.Lens' CreatePortal Prelude.Text
createPortal_portalContactEmail = Lens.lens (\CreatePortal' {portalContactEmail} -> portalContactEmail) (\s@CreatePortal' {} a -> s {portalContactEmail = a} :: CreatePortal)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of a service role that allows the portal\'s users to access your IoT
-- SiteWise resources on your behalf. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/monitor-service-role.html Using service roles for IoT SiteWise Monitor>
-- in the /IoT SiteWise User Guide/.
createPortal_roleArn :: Lens.Lens' CreatePortal Prelude.Text
createPortal_roleArn = Lens.lens (\CreatePortal' {roleArn} -> roleArn) (\s@CreatePortal' {} a -> s {roleArn = a} :: CreatePortal)

instance Core.AWSRequest CreatePortal where
  type AWSResponse CreatePortal = CreatePortalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalId")
            Prelude.<*> (x Data..:> "portalArn")
            Prelude.<*> (x Data..:> "portalStartUrl")
            Prelude.<*> (x Data..:> "portalStatus")
            Prelude.<*> (x Data..:> "ssoApplicationId")
      )

instance Prelude.Hashable CreatePortal where
  hashWithSalt _salt CreatePortal' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` alarms
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` portalDescription
      `Prelude.hashWithSalt` portalAuthMode
      `Prelude.hashWithSalt` portalLogoImageFile
      `Prelude.hashWithSalt` notificationSenderEmail
      `Prelude.hashWithSalt` portalName
      `Prelude.hashWithSalt` portalContactEmail
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreatePortal where
  rnf CreatePortal' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf portalDescription
      `Prelude.seq` Prelude.rnf portalAuthMode
      `Prelude.seq` Prelude.rnf portalLogoImageFile
      `Prelude.seq` Prelude.rnf notificationSenderEmail
      `Prelude.seq` Prelude.rnf portalName
      `Prelude.seq` Prelude.rnf portalContactEmail
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreatePortal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePortal where
  toJSON CreatePortal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("alarms" Data..=) Prelude.<$> alarms,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("portalDescription" Data..=)
              Prelude.<$> portalDescription,
            ("portalAuthMode" Data..=)
              Prelude.<$> portalAuthMode,
            ("portalLogoImageFile" Data..=)
              Prelude.<$> portalLogoImageFile,
            ("notificationSenderEmail" Data..=)
              Prelude.<$> notificationSenderEmail,
            Prelude.Just ("portalName" Data..= portalName),
            Prelude.Just
              ("portalContactEmail" Data..= portalContactEmail),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreatePortal where
  toPath = Prelude.const "/portals"

instance Data.ToQuery CreatePortal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePortalResponse' smart constructor.
data CreatePortalResponse = CreatePortalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the created portal.
    portalId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the portal, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:portal\/${PortalId}@
    portalArn :: Prelude.Text,
    -- | The URL for the IoT SiteWise Monitor portal. You can use this URL to
    -- access portals that use IAM Identity Center for authentication. For
    -- portals that use IAM for authentication, you must use the IoT SiteWise
    -- console to get a URL that you can use to access the portal.
    portalStartUrl :: Prelude.Text,
    -- | The status of the portal, which contains a state (@CREATING@ after
    -- successfully calling this operation) and any error message.
    portalStatus :: PortalStatus,
    -- | The associated IAM Identity Center application ID, if the portal uses
    -- IAM Identity Center.
    ssoApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePortalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPortalResponse_httpStatus' - The response's http status code.
--
-- 'portalId', 'createPortalResponse_portalId' - The ID of the created portal.
--
-- 'portalArn', 'createPortalResponse_portalArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the portal, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:portal\/${PortalId}@
--
-- 'portalStartUrl', 'createPortalResponse_portalStartUrl' - The URL for the IoT SiteWise Monitor portal. You can use this URL to
-- access portals that use IAM Identity Center for authentication. For
-- portals that use IAM for authentication, you must use the IoT SiteWise
-- console to get a URL that you can use to access the portal.
--
-- 'portalStatus', 'createPortalResponse_portalStatus' - The status of the portal, which contains a state (@CREATING@ after
-- successfully calling this operation) and any error message.
--
-- 'ssoApplicationId', 'createPortalResponse_ssoApplicationId' - The associated IAM Identity Center application ID, if the portal uses
-- IAM Identity Center.
newCreatePortalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalId'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'portalStartUrl'
  Prelude.Text ->
  -- | 'portalStatus'
  PortalStatus ->
  -- | 'ssoApplicationId'
  Prelude.Text ->
  CreatePortalResponse
newCreatePortalResponse
  pHttpStatus_
  pPortalId_
  pPortalArn_
  pPortalStartUrl_
  pPortalStatus_
  pSsoApplicationId_ =
    CreatePortalResponse'
      { httpStatus = pHttpStatus_,
        portalId = pPortalId_,
        portalArn = pPortalArn_,
        portalStartUrl = pPortalStartUrl_,
        portalStatus = pPortalStatus_,
        ssoApplicationId = pSsoApplicationId_
      }

-- | The response's http status code.
createPortalResponse_httpStatus :: Lens.Lens' CreatePortalResponse Prelude.Int
createPortalResponse_httpStatus = Lens.lens (\CreatePortalResponse' {httpStatus} -> httpStatus) (\s@CreatePortalResponse' {} a -> s {httpStatus = a} :: CreatePortalResponse)

-- | The ID of the created portal.
createPortalResponse_portalId :: Lens.Lens' CreatePortalResponse Prelude.Text
createPortalResponse_portalId = Lens.lens (\CreatePortalResponse' {portalId} -> portalId) (\s@CreatePortalResponse' {} a -> s {portalId = a} :: CreatePortalResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the portal, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:portal\/${PortalId}@
createPortalResponse_portalArn :: Lens.Lens' CreatePortalResponse Prelude.Text
createPortalResponse_portalArn = Lens.lens (\CreatePortalResponse' {portalArn} -> portalArn) (\s@CreatePortalResponse' {} a -> s {portalArn = a} :: CreatePortalResponse)

-- | The URL for the IoT SiteWise Monitor portal. You can use this URL to
-- access portals that use IAM Identity Center for authentication. For
-- portals that use IAM for authentication, you must use the IoT SiteWise
-- console to get a URL that you can use to access the portal.
createPortalResponse_portalStartUrl :: Lens.Lens' CreatePortalResponse Prelude.Text
createPortalResponse_portalStartUrl = Lens.lens (\CreatePortalResponse' {portalStartUrl} -> portalStartUrl) (\s@CreatePortalResponse' {} a -> s {portalStartUrl = a} :: CreatePortalResponse)

-- | The status of the portal, which contains a state (@CREATING@ after
-- successfully calling this operation) and any error message.
createPortalResponse_portalStatus :: Lens.Lens' CreatePortalResponse PortalStatus
createPortalResponse_portalStatus = Lens.lens (\CreatePortalResponse' {portalStatus} -> portalStatus) (\s@CreatePortalResponse' {} a -> s {portalStatus = a} :: CreatePortalResponse)

-- | The associated IAM Identity Center application ID, if the portal uses
-- IAM Identity Center.
createPortalResponse_ssoApplicationId :: Lens.Lens' CreatePortalResponse Prelude.Text
createPortalResponse_ssoApplicationId = Lens.lens (\CreatePortalResponse' {ssoApplicationId} -> ssoApplicationId) (\s@CreatePortalResponse' {} a -> s {ssoApplicationId = a} :: CreatePortalResponse)

instance Prelude.NFData CreatePortalResponse where
  rnf CreatePortalResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf portalId
      `Prelude.seq` Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf portalStartUrl
      `Prelude.seq` Prelude.rnf portalStatus
      `Prelude.seq` Prelude.rnf ssoApplicationId
