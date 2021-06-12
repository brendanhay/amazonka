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
-- Module      : Network.AWS.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the gateway you previously deployed on your host. In the
-- activation process, you specify information such as the AWS Region that
-- you want to use for storing snapshots or tapes, the time zone for
-- scheduled snapshots the gateway snapshot schedule window, an activation
-- key, and a name for your gateway. The activation process also associates
-- your gateway with your account. For more information, see
-- UpdateGatewayInformation.
--
-- You must turn on the gateway VM before you can activate your gateway.
module Network.AWS.StorageGateway.ActivateGateway
  ( -- * Creating a Request
    ActivateGateway (..),
    newActivateGateway,

    -- * Request Lenses
    activateGateway_tapeDriveType,
    activateGateway_gatewayType,
    activateGateway_mediumChangerType,
    activateGateway_tags,
    activateGateway_activationKey,
    activateGateway_gatewayName,
    activateGateway_gatewayTimezone,
    activateGateway_gatewayRegion,

    -- * Destructuring the Response
    ActivateGatewayResponse (..),
    newActivateGatewayResponse,

    -- * Response Lenses
    activateGatewayResponse_gatewayARN,
    activateGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   ActivateGatewayInput$ActivationKey
--
-- -   ActivateGatewayInput$GatewayName
--
-- -   ActivateGatewayInput$GatewayRegion
--
-- -   ActivateGatewayInput$GatewayTimezone
--
-- -   ActivateGatewayInput$GatewayType
--
-- -   ActivateGatewayInput$MediumChangerType
--
-- -   ActivateGatewayInput$TapeDriveType
--
-- /See:/ 'newActivateGateway' smart constructor.
data ActivateGateway = ActivateGateway'
  { -- | The value that indicates the type of tape drive to use for tape gateway.
    -- This field is optional.
    --
    -- Valid Values: @IBM-ULT3580-TD5@
    tapeDriveType :: Core.Maybe Core.Text,
    -- | A value that defines the type of gateway to activate. The type specified
    -- is critical to all later functions of the gateway and cannot be changed
    -- after activation. The default value is @CACHED@.
    --
    -- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@
    gatewayType :: Core.Maybe Core.Text,
    -- | The value that indicates the type of medium changer to use for tape
    -- gateway. This field is optional.
    --
    -- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
    mediumChangerType :: Core.Maybe Core.Text,
    -- | A list of up to 50 tags that you can assign to the gateway. Each tag is
    -- a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers that
    -- can be represented in UTF-8 format, and the following special
    -- characters: + - = . _ : \/ \@. The maximum length of a tag\'s key is 128
    -- characters, and the maximum length for a tag\'s value is 256 characters.
    tags :: Core.Maybe [Tag],
    -- | Your gateway activation key. You can obtain the activation key by
    -- sending an HTTP GET request with redirects enabled to the gateway IP
    -- address (port 80). The redirect URL returned in the response provides
    -- you the activation key for your gateway in the query string parameter
    -- @activationKey@. It may also include other activation-related
    -- parameters, however, these are merely defaults -- the arguments you pass
    -- to the @ActivateGateway@ API call determine the actual configuration of
    -- your gateway.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key>
    -- in the /AWS Storage Gateway User Guide/.
    activationKey :: Core.Text,
    -- | The name you configured for your gateway.
    gatewayName :: Core.Text,
    -- | A value that indicates the time zone you want to set for the gateway.
    -- The time zone is of the format \"GMT-hr:mm\" or \"GMT+hr:mm\". For
    -- example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00
    -- indicates the time is 2 hours ahead of GMT. The time zone is used, for
    -- example, for scheduling snapshots and your gateway\'s maintenance
    -- schedule.
    gatewayTimezone :: Core.Text,
    -- | A value that indicates the AWS Region where you want to store your data.
    -- The gateway AWS Region specified must be the same AWS Region as the AWS
    -- Region in your @Host@ header in the request. For more information about
    -- available AWS Regions and endpoints for AWS Storage Gateway, see
    -- <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas>
    -- in the /AWS General Reference/.
    --
    -- Valid Values: See
    -- <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas>
    -- in the /AWS General Reference/.
    gatewayRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivateGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeDriveType', 'activateGateway_tapeDriveType' - The value that indicates the type of tape drive to use for tape gateway.
-- This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@
--
-- 'gatewayType', 'activateGateway_gatewayType' - A value that defines the type of gateway to activate. The type specified
-- is critical to all later functions of the gateway and cannot be changed
-- after activation. The default value is @CACHED@.
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@
--
-- 'mediumChangerType', 'activateGateway_mediumChangerType' - The value that indicates the type of medium changer to use for tape
-- gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
--
-- 'tags', 'activateGateway_tags' - A list of up to 50 tags that you can assign to the gateway. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers that
-- can be represented in UTF-8 format, and the following special
-- characters: + - = . _ : \/ \@. The maximum length of a tag\'s key is 128
-- characters, and the maximum length for a tag\'s value is 256 characters.
--
-- 'activationKey', 'activateGateway_activationKey' - Your gateway activation key. You can obtain the activation key by
-- sending an HTTP GET request with redirects enabled to the gateway IP
-- address (port 80). The redirect URL returned in the response provides
-- you the activation key for your gateway in the query string parameter
-- @activationKey@. It may also include other activation-related
-- parameters, however, these are merely defaults -- the arguments you pass
-- to the @ActivateGateway@ API call determine the actual configuration of
-- your gateway.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key>
-- in the /AWS Storage Gateway User Guide/.
--
-- 'gatewayName', 'activateGateway_gatewayName' - The name you configured for your gateway.
--
-- 'gatewayTimezone', 'activateGateway_gatewayTimezone' - A value that indicates the time zone you want to set for the gateway.
-- The time zone is of the format \"GMT-hr:mm\" or \"GMT+hr:mm\". For
-- example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00
-- indicates the time is 2 hours ahead of GMT. The time zone is used, for
-- example, for scheduling snapshots and your gateway\'s maintenance
-- schedule.
--
-- 'gatewayRegion', 'activateGateway_gatewayRegion' - A value that indicates the AWS Region where you want to store your data.
-- The gateway AWS Region specified must be the same AWS Region as the AWS
-- Region in your @Host@ header in the request. For more information about
-- available AWS Regions and endpoints for AWS Storage Gateway, see
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas>
-- in the /AWS General Reference/.
--
-- Valid Values: See
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas>
-- in the /AWS General Reference/.
newActivateGateway ::
  -- | 'activationKey'
  Core.Text ->
  -- | 'gatewayName'
  Core.Text ->
  -- | 'gatewayTimezone'
  Core.Text ->
  -- | 'gatewayRegion'
  Core.Text ->
  ActivateGateway
newActivateGateway
  pActivationKey_
  pGatewayName_
  pGatewayTimezone_
  pGatewayRegion_ =
    ActivateGateway'
      { tapeDriveType = Core.Nothing,
        gatewayType = Core.Nothing,
        mediumChangerType = Core.Nothing,
        tags = Core.Nothing,
        activationKey = pActivationKey_,
        gatewayName = pGatewayName_,
        gatewayTimezone = pGatewayTimezone_,
        gatewayRegion = pGatewayRegion_
      }

-- | The value that indicates the type of tape drive to use for tape gateway.
-- This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@
activateGateway_tapeDriveType :: Lens.Lens' ActivateGateway (Core.Maybe Core.Text)
activateGateway_tapeDriveType = Lens.lens (\ActivateGateway' {tapeDriveType} -> tapeDriveType) (\s@ActivateGateway' {} a -> s {tapeDriveType = a} :: ActivateGateway)

-- | A value that defines the type of gateway to activate. The type specified
-- is critical to all later functions of the gateway and cannot be changed
-- after activation. The default value is @CACHED@.
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@
activateGateway_gatewayType :: Lens.Lens' ActivateGateway (Core.Maybe Core.Text)
activateGateway_gatewayType = Lens.lens (\ActivateGateway' {gatewayType} -> gatewayType) (\s@ActivateGateway' {} a -> s {gatewayType = a} :: ActivateGateway)

-- | The value that indicates the type of medium changer to use for tape
-- gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
activateGateway_mediumChangerType :: Lens.Lens' ActivateGateway (Core.Maybe Core.Text)
activateGateway_mediumChangerType = Lens.lens (\ActivateGateway' {mediumChangerType} -> mediumChangerType) (\s@ActivateGateway' {} a -> s {mediumChangerType = a} :: ActivateGateway)

-- | A list of up to 50 tags that you can assign to the gateway. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers that
-- can be represented in UTF-8 format, and the following special
-- characters: + - = . _ : \/ \@. The maximum length of a tag\'s key is 128
-- characters, and the maximum length for a tag\'s value is 256 characters.
activateGateway_tags :: Lens.Lens' ActivateGateway (Core.Maybe [Tag])
activateGateway_tags = Lens.lens (\ActivateGateway' {tags} -> tags) (\s@ActivateGateway' {} a -> s {tags = a} :: ActivateGateway) Core.. Lens.mapping Lens._Coerce

-- | Your gateway activation key. You can obtain the activation key by
-- sending an HTTP GET request with redirects enabled to the gateway IP
-- address (port 80). The redirect URL returned in the response provides
-- you the activation key for your gateway in the query string parameter
-- @activationKey@. It may also include other activation-related
-- parameters, however, these are merely defaults -- the arguments you pass
-- to the @ActivateGateway@ API call determine the actual configuration of
-- your gateway.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key>
-- in the /AWS Storage Gateway User Guide/.
activateGateway_activationKey :: Lens.Lens' ActivateGateway Core.Text
activateGateway_activationKey = Lens.lens (\ActivateGateway' {activationKey} -> activationKey) (\s@ActivateGateway' {} a -> s {activationKey = a} :: ActivateGateway)

-- | The name you configured for your gateway.
activateGateway_gatewayName :: Lens.Lens' ActivateGateway Core.Text
activateGateway_gatewayName = Lens.lens (\ActivateGateway' {gatewayName} -> gatewayName) (\s@ActivateGateway' {} a -> s {gatewayName = a} :: ActivateGateway)

-- | A value that indicates the time zone you want to set for the gateway.
-- The time zone is of the format \"GMT-hr:mm\" or \"GMT+hr:mm\". For
-- example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00
-- indicates the time is 2 hours ahead of GMT. The time zone is used, for
-- example, for scheduling snapshots and your gateway\'s maintenance
-- schedule.
activateGateway_gatewayTimezone :: Lens.Lens' ActivateGateway Core.Text
activateGateway_gatewayTimezone = Lens.lens (\ActivateGateway' {gatewayTimezone} -> gatewayTimezone) (\s@ActivateGateway' {} a -> s {gatewayTimezone = a} :: ActivateGateway)

-- | A value that indicates the AWS Region where you want to store your data.
-- The gateway AWS Region specified must be the same AWS Region as the AWS
-- Region in your @Host@ header in the request. For more information about
-- available AWS Regions and endpoints for AWS Storage Gateway, see
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas>
-- in the /AWS General Reference/.
--
-- Valid Values: See
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas>
-- in the /AWS General Reference/.
activateGateway_gatewayRegion :: Lens.Lens' ActivateGateway Core.Text
activateGateway_gatewayRegion = Lens.lens (\ActivateGateway' {gatewayRegion} -> gatewayRegion) (\s@ActivateGateway' {} a -> s {gatewayRegion = a} :: ActivateGateway)

instance Core.AWSRequest ActivateGateway where
  type
    AWSResponse ActivateGateway =
      ActivateGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateGatewayResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ActivateGateway

instance Core.NFData ActivateGateway

instance Core.ToHeaders ActivateGateway where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.ActivateGateway" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ActivateGateway where
  toJSON ActivateGateway' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TapeDriveType" Core..=) Core.<$> tapeDriveType,
            ("GatewayType" Core..=) Core.<$> gatewayType,
            ("MediumChangerType" Core..=)
              Core.<$> mediumChangerType,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("ActivationKey" Core..= activationKey),
            Core.Just ("GatewayName" Core..= gatewayName),
            Core.Just
              ("GatewayTimezone" Core..= gatewayTimezone),
            Core.Just ("GatewayRegion" Core..= gatewayRegion)
          ]
      )

instance Core.ToPath ActivateGateway where
  toPath = Core.const "/"

instance Core.ToQuery ActivateGateway where
  toQuery = Core.const Core.mempty

-- | AWS Storage Gateway returns the Amazon Resource Name (ARN) of the
-- activated gateway. It is a string made of information such as your
-- account, gateway name, and AWS Region. This ARN is used to reference the
-- gateway in other API operations as well as resource-based authorization.
--
-- For gateways activated prior to September 02, 2015, the gateway ARN
-- contains the gateway name rather than the gateway ID. Changing the name
-- of the gateway has no effect on the gateway ARN.
--
-- /See:/ 'newActivateGatewayResponse' smart constructor.
data ActivateGatewayResponse = ActivateGatewayResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'activateGatewayResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'activateGatewayResponse_httpStatus' - The response's http status code.
newActivateGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ActivateGatewayResponse
newActivateGatewayResponse pHttpStatus_ =
  ActivateGatewayResponse'
    { gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
activateGatewayResponse_gatewayARN :: Lens.Lens' ActivateGatewayResponse (Core.Maybe Core.Text)
activateGatewayResponse_gatewayARN = Lens.lens (\ActivateGatewayResponse' {gatewayARN} -> gatewayARN) (\s@ActivateGatewayResponse' {} a -> s {gatewayARN = a} :: ActivateGatewayResponse)

-- | The response's http status code.
activateGatewayResponse_httpStatus :: Lens.Lens' ActivateGatewayResponse Core.Int
activateGatewayResponse_httpStatus = Lens.lens (\ActivateGatewayResponse' {httpStatus} -> httpStatus) (\s@ActivateGatewayResponse' {} a -> s {httpStatus = a} :: ActivateGatewayResponse)

instance Core.NFData ActivateGatewayResponse
