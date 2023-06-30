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
-- Module      : Amazonka.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the gateway you previously deployed on your host. In the
-- activation process, you specify information such as the Amazon Web
-- Services Region that you want to use for storing snapshots or tapes, the
-- time zone for scheduled snapshots the gateway snapshot schedule window,
-- an activation key, and a name for your gateway. The activation process
-- also associates your gateway with your account. For more information,
-- see UpdateGatewayInformation.
--
-- You must turn on the gateway VM before you can activate your gateway.
module Amazonka.StorageGateway.ActivateGateway
  ( -- * Creating a Request
    ActivateGateway (..),
    newActivateGateway,

    -- * Request Lenses
    activateGateway_gatewayType,
    activateGateway_mediumChangerType,
    activateGateway_tags,
    activateGateway_tapeDriveType,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

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
  { -- | A value that defines the type of gateway to activate. The type specified
    -- is critical to all later functions of the gateway and cannot be changed
    -- after activation. The default value is @CACHED@.
    --
    -- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @VTL_SNOW@ | @FILE_S3@ |
    -- @FILE_FSX_SMB@
    gatewayType :: Prelude.Maybe Prelude.Text,
    -- | The value that indicates the type of medium changer to use for tape
    -- gateway. This field is optional.
    --
    -- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
    mediumChangerType :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags that you can assign to the gateway. Each tag is
    -- a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers that
    -- can be represented in UTF-8 format, and the following special
    -- characters: + - = . _ : \/ \@. The maximum length of a tag\'s key is 128
    -- characters, and the maximum length for a tag\'s value is 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | The value that indicates the type of tape drive to use for tape gateway.
    -- This field is optional.
    --
    -- Valid Values: @IBM-ULT3580-TD5@
    tapeDriveType :: Prelude.Maybe Prelude.Text,
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
    -- in the /Storage Gateway User Guide/.
    activationKey :: Prelude.Text,
    -- | The name you configured for your gateway.
    gatewayName :: Prelude.Text,
    -- | A value that indicates the time zone you want to set for the gateway.
    -- The time zone is of the format \"GMT-hr:mm\" or \"GMT+hr:mm\". For
    -- example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00
    -- indicates the time is 2 hours ahead of GMT. The time zone is used, for
    -- example, for scheduling snapshots and your gateway\'s maintenance
    -- schedule.
    gatewayTimezone :: Prelude.Text,
    -- | A value that indicates the Amazon Web Services Region where you want to
    -- store your data. The gateway Amazon Web Services Region specified must
    -- be the same Amazon Web Services Region as the Amazon Web Services Region
    -- in your @Host@ header in the request. For more information about
    -- available Amazon Web Services Regions and endpoints for Storage Gateway,
    -- see
    -- <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>
    -- in the /Amazon Web Services General Reference/.
    --
    -- Valid Values: See
    -- <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>
    -- in the /Amazon Web Services General Reference/.
    gatewayRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayType', 'activateGateway_gatewayType' - A value that defines the type of gateway to activate. The type specified
-- is critical to all later functions of the gateway and cannot be changed
-- after activation. The default value is @CACHED@.
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @VTL_SNOW@ | @FILE_S3@ |
-- @FILE_FSX_SMB@
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
-- 'tapeDriveType', 'activateGateway_tapeDriveType' - The value that indicates the type of tape drive to use for tape gateway.
-- This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@
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
-- in the /Storage Gateway User Guide/.
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
-- 'gatewayRegion', 'activateGateway_gatewayRegion' - A value that indicates the Amazon Web Services Region where you want to
-- store your data. The gateway Amazon Web Services Region specified must
-- be the same Amazon Web Services Region as the Amazon Web Services Region
-- in your @Host@ header in the request. For more information about
-- available Amazon Web Services Regions and endpoints for Storage Gateway,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
--
-- Valid Values: See
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
newActivateGateway ::
  -- | 'activationKey'
  Prelude.Text ->
  -- | 'gatewayName'
  Prelude.Text ->
  -- | 'gatewayTimezone'
  Prelude.Text ->
  -- | 'gatewayRegion'
  Prelude.Text ->
  ActivateGateway
newActivateGateway
  pActivationKey_
  pGatewayName_
  pGatewayTimezone_
  pGatewayRegion_ =
    ActivateGateway'
      { gatewayType = Prelude.Nothing,
        mediumChangerType = Prelude.Nothing,
        tags = Prelude.Nothing,
        tapeDriveType = Prelude.Nothing,
        activationKey = pActivationKey_,
        gatewayName = pGatewayName_,
        gatewayTimezone = pGatewayTimezone_,
        gatewayRegion = pGatewayRegion_
      }

-- | A value that defines the type of gateway to activate. The type specified
-- is critical to all later functions of the gateway and cannot be changed
-- after activation. The default value is @CACHED@.
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @VTL_SNOW@ | @FILE_S3@ |
-- @FILE_FSX_SMB@
activateGateway_gatewayType :: Lens.Lens' ActivateGateway (Prelude.Maybe Prelude.Text)
activateGateway_gatewayType = Lens.lens (\ActivateGateway' {gatewayType} -> gatewayType) (\s@ActivateGateway' {} a -> s {gatewayType = a} :: ActivateGateway)

-- | The value that indicates the type of medium changer to use for tape
-- gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
activateGateway_mediumChangerType :: Lens.Lens' ActivateGateway (Prelude.Maybe Prelude.Text)
activateGateway_mediumChangerType = Lens.lens (\ActivateGateway' {mediumChangerType} -> mediumChangerType) (\s@ActivateGateway' {} a -> s {mediumChangerType = a} :: ActivateGateway)

-- | A list of up to 50 tags that you can assign to the gateway. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers that
-- can be represented in UTF-8 format, and the following special
-- characters: + - = . _ : \/ \@. The maximum length of a tag\'s key is 128
-- characters, and the maximum length for a tag\'s value is 256 characters.
activateGateway_tags :: Lens.Lens' ActivateGateway (Prelude.Maybe [Tag])
activateGateway_tags = Lens.lens (\ActivateGateway' {tags} -> tags) (\s@ActivateGateway' {} a -> s {tags = a} :: ActivateGateway) Prelude.. Lens.mapping Lens.coerced

-- | The value that indicates the type of tape drive to use for tape gateway.
-- This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@
activateGateway_tapeDriveType :: Lens.Lens' ActivateGateway (Prelude.Maybe Prelude.Text)
activateGateway_tapeDriveType = Lens.lens (\ActivateGateway' {tapeDriveType} -> tapeDriveType) (\s@ActivateGateway' {} a -> s {tapeDriveType = a} :: ActivateGateway)

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
-- in the /Storage Gateway User Guide/.
activateGateway_activationKey :: Lens.Lens' ActivateGateway Prelude.Text
activateGateway_activationKey = Lens.lens (\ActivateGateway' {activationKey} -> activationKey) (\s@ActivateGateway' {} a -> s {activationKey = a} :: ActivateGateway)

-- | The name you configured for your gateway.
activateGateway_gatewayName :: Lens.Lens' ActivateGateway Prelude.Text
activateGateway_gatewayName = Lens.lens (\ActivateGateway' {gatewayName} -> gatewayName) (\s@ActivateGateway' {} a -> s {gatewayName = a} :: ActivateGateway)

-- | A value that indicates the time zone you want to set for the gateway.
-- The time zone is of the format \"GMT-hr:mm\" or \"GMT+hr:mm\". For
-- example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00
-- indicates the time is 2 hours ahead of GMT. The time zone is used, for
-- example, for scheduling snapshots and your gateway\'s maintenance
-- schedule.
activateGateway_gatewayTimezone :: Lens.Lens' ActivateGateway Prelude.Text
activateGateway_gatewayTimezone = Lens.lens (\ActivateGateway' {gatewayTimezone} -> gatewayTimezone) (\s@ActivateGateway' {} a -> s {gatewayTimezone = a} :: ActivateGateway)

-- | A value that indicates the Amazon Web Services Region where you want to
-- store your data. The gateway Amazon Web Services Region specified must
-- be the same Amazon Web Services Region as the Amazon Web Services Region
-- in your @Host@ header in the request. For more information about
-- available Amazon Web Services Regions and endpoints for Storage Gateway,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
--
-- Valid Values: See
-- <https://docs.aws.amazon.com/general/latest/gr/sg.html Storage Gateway endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
activateGateway_gatewayRegion :: Lens.Lens' ActivateGateway Prelude.Text
activateGateway_gatewayRegion = Lens.lens (\ActivateGateway' {gatewayRegion} -> gatewayRegion) (\s@ActivateGateway' {} a -> s {gatewayRegion = a} :: ActivateGateway)

instance Core.AWSRequest ActivateGateway where
  type
    AWSResponse ActivateGateway =
      ActivateGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateGatewayResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivateGateway where
  hashWithSalt _salt ActivateGateway' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayType
      `Prelude.hashWithSalt` mediumChangerType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tapeDriveType
      `Prelude.hashWithSalt` activationKey
      `Prelude.hashWithSalt` gatewayName
      `Prelude.hashWithSalt` gatewayTimezone
      `Prelude.hashWithSalt` gatewayRegion

instance Prelude.NFData ActivateGateway where
  rnf ActivateGateway' {..} =
    Prelude.rnf gatewayType
      `Prelude.seq` Prelude.rnf mediumChangerType
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tapeDriveType
      `Prelude.seq` Prelude.rnf activationKey
      `Prelude.seq` Prelude.rnf gatewayName
      `Prelude.seq` Prelude.rnf gatewayTimezone
      `Prelude.seq` Prelude.rnf gatewayRegion

instance Data.ToHeaders ActivateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.ActivateGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ActivateGateway where
  toJSON ActivateGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GatewayType" Data..=) Prelude.<$> gatewayType,
            ("MediumChangerType" Data..=)
              Prelude.<$> mediumChangerType,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TapeDriveType" Data..=) Prelude.<$> tapeDriveType,
            Prelude.Just ("ActivationKey" Data..= activationKey),
            Prelude.Just ("GatewayName" Data..= gatewayName),
            Prelude.Just
              ("GatewayTimezone" Data..= gatewayTimezone),
            Prelude.Just
              ("GatewayRegion" Data..= gatewayRegion)
          ]
      )

instance Data.ToPath ActivateGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery ActivateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | Storage Gateway returns the Amazon Resource Name (ARN) of the activated
-- gateway. It is a string made of information such as your account,
-- gateway name, and Amazon Web Services Region. This ARN is used to
-- reference the gateway in other API operations as well as resource-based
-- authorization.
--
-- For gateways activated prior to September 02, 2015, the gateway ARN
-- contains the gateway name rather than the gateway ID. Changing the name
-- of the gateway has no effect on the gateway ARN.
--
-- /See:/ 'newActivateGatewayResponse' smart constructor.
data ActivateGatewayResponse = ActivateGatewayResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ActivateGatewayResponse
newActivateGatewayResponse pHttpStatus_ =
  ActivateGatewayResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
activateGatewayResponse_gatewayARN :: Lens.Lens' ActivateGatewayResponse (Prelude.Maybe Prelude.Text)
activateGatewayResponse_gatewayARN = Lens.lens (\ActivateGatewayResponse' {gatewayARN} -> gatewayARN) (\s@ActivateGatewayResponse' {} a -> s {gatewayARN = a} :: ActivateGatewayResponse)

-- | The response's http status code.
activateGatewayResponse_httpStatus :: Lens.Lens' ActivateGatewayResponse Prelude.Int
activateGatewayResponse_httpStatus = Lens.lens (\ActivateGatewayResponse' {httpStatus} -> httpStatus) (\s@ActivateGatewayResponse' {} a -> s {httpStatus = a} :: ActivateGatewayResponse)

instance Prelude.NFData ActivateGatewayResponse where
  rnf ActivateGatewayResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
