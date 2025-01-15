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
-- Module      : Amazonka.PrivateNetworks.ConfigureAccessPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the specified network resource.
--
-- Use this action to specify the geographic position of the hardware. You
-- must provide Certified Professional Installer (CPI) credentials in the
-- request so that we can obtain spectrum grants. For more information, see
-- <https://docs.aws.amazon.com/private-networks/latest/userguide/radio-units.html Radio units>
-- in the /Amazon Web Services Private 5G User Guide/.
module Amazonka.PrivateNetworks.ConfigureAccessPoint
  ( -- * Creating a Request
    ConfigureAccessPoint (..),
    newConfigureAccessPoint,

    -- * Request Lenses
    configureAccessPoint_cpiSecretKey,
    configureAccessPoint_cpiUserId,
    configureAccessPoint_cpiUserPassword,
    configureAccessPoint_cpiUsername,
    configureAccessPoint_position,
    configureAccessPoint_accessPointArn,

    -- * Destructuring the Response
    ConfigureAccessPointResponse (..),
    newConfigureAccessPointResponse,

    -- * Response Lenses
    configureAccessPointResponse_httpStatus,
    configureAccessPointResponse_accessPoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConfigureAccessPoint' smart constructor.
data ConfigureAccessPoint = ConfigureAccessPoint'
  { -- | A Base64 encoded string of the CPI certificate associated with the CPI
    -- user who is certifying the coordinates of the network resource.
    cpiSecretKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The CPI user ID of the CPI user who is certifying the coordinates of the
    -- network resource.
    cpiUserId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The CPI password associated with the CPI certificate in @cpiSecretKey@.
    cpiUserPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The CPI user name of the CPI user who is certifying the coordinates of
    -- the radio unit.
    cpiUsername :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The position of the network resource.
    position :: Prelude.Maybe Position,
    -- | The Amazon Resource Name (ARN) of the network resource.
    accessPointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureAccessPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpiSecretKey', 'configureAccessPoint_cpiSecretKey' - A Base64 encoded string of the CPI certificate associated with the CPI
-- user who is certifying the coordinates of the network resource.
--
-- 'cpiUserId', 'configureAccessPoint_cpiUserId' - The CPI user ID of the CPI user who is certifying the coordinates of the
-- network resource.
--
-- 'cpiUserPassword', 'configureAccessPoint_cpiUserPassword' - The CPI password associated with the CPI certificate in @cpiSecretKey@.
--
-- 'cpiUsername', 'configureAccessPoint_cpiUsername' - The CPI user name of the CPI user who is certifying the coordinates of
-- the radio unit.
--
-- 'position', 'configureAccessPoint_position' - The position of the network resource.
--
-- 'accessPointArn', 'configureAccessPoint_accessPointArn' - The Amazon Resource Name (ARN) of the network resource.
newConfigureAccessPoint ::
  -- | 'accessPointArn'
  Prelude.Text ->
  ConfigureAccessPoint
newConfigureAccessPoint pAccessPointArn_ =
  ConfigureAccessPoint'
    { cpiSecretKey =
        Prelude.Nothing,
      cpiUserId = Prelude.Nothing,
      cpiUserPassword = Prelude.Nothing,
      cpiUsername = Prelude.Nothing,
      position = Prelude.Nothing,
      accessPointArn = pAccessPointArn_
    }

-- | A Base64 encoded string of the CPI certificate associated with the CPI
-- user who is certifying the coordinates of the network resource.
configureAccessPoint_cpiSecretKey :: Lens.Lens' ConfigureAccessPoint (Prelude.Maybe Prelude.Text)
configureAccessPoint_cpiSecretKey = Lens.lens (\ConfigureAccessPoint' {cpiSecretKey} -> cpiSecretKey) (\s@ConfigureAccessPoint' {} a -> s {cpiSecretKey = a} :: ConfigureAccessPoint) Prelude.. Lens.mapping Data._Sensitive

-- | The CPI user ID of the CPI user who is certifying the coordinates of the
-- network resource.
configureAccessPoint_cpiUserId :: Lens.Lens' ConfigureAccessPoint (Prelude.Maybe Prelude.Text)
configureAccessPoint_cpiUserId = Lens.lens (\ConfigureAccessPoint' {cpiUserId} -> cpiUserId) (\s@ConfigureAccessPoint' {} a -> s {cpiUserId = a} :: ConfigureAccessPoint) Prelude.. Lens.mapping Data._Sensitive

-- | The CPI password associated with the CPI certificate in @cpiSecretKey@.
configureAccessPoint_cpiUserPassword :: Lens.Lens' ConfigureAccessPoint (Prelude.Maybe Prelude.Text)
configureAccessPoint_cpiUserPassword = Lens.lens (\ConfigureAccessPoint' {cpiUserPassword} -> cpiUserPassword) (\s@ConfigureAccessPoint' {} a -> s {cpiUserPassword = a} :: ConfigureAccessPoint) Prelude.. Lens.mapping Data._Sensitive

-- | The CPI user name of the CPI user who is certifying the coordinates of
-- the radio unit.
configureAccessPoint_cpiUsername :: Lens.Lens' ConfigureAccessPoint (Prelude.Maybe Prelude.Text)
configureAccessPoint_cpiUsername = Lens.lens (\ConfigureAccessPoint' {cpiUsername} -> cpiUsername) (\s@ConfigureAccessPoint' {} a -> s {cpiUsername = a} :: ConfigureAccessPoint) Prelude.. Lens.mapping Data._Sensitive

-- | The position of the network resource.
configureAccessPoint_position :: Lens.Lens' ConfigureAccessPoint (Prelude.Maybe Position)
configureAccessPoint_position = Lens.lens (\ConfigureAccessPoint' {position} -> position) (\s@ConfigureAccessPoint' {} a -> s {position = a} :: ConfigureAccessPoint)

-- | The Amazon Resource Name (ARN) of the network resource.
configureAccessPoint_accessPointArn :: Lens.Lens' ConfigureAccessPoint Prelude.Text
configureAccessPoint_accessPointArn = Lens.lens (\ConfigureAccessPoint' {accessPointArn} -> accessPointArn) (\s@ConfigureAccessPoint' {} a -> s {accessPointArn = a} :: ConfigureAccessPoint)

instance Core.AWSRequest ConfigureAccessPoint where
  type
    AWSResponse ConfigureAccessPoint =
      ConfigureAccessPointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfigureAccessPointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "accessPoint")
      )

instance Prelude.Hashable ConfigureAccessPoint where
  hashWithSalt _salt ConfigureAccessPoint' {..} =
    _salt
      `Prelude.hashWithSalt` cpiSecretKey
      `Prelude.hashWithSalt` cpiUserId
      `Prelude.hashWithSalt` cpiUserPassword
      `Prelude.hashWithSalt` cpiUsername
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` accessPointArn

instance Prelude.NFData ConfigureAccessPoint where
  rnf ConfigureAccessPoint' {..} =
    Prelude.rnf cpiSecretKey `Prelude.seq`
      Prelude.rnf cpiUserId `Prelude.seq`
        Prelude.rnf cpiUserPassword `Prelude.seq`
          Prelude.rnf cpiUsername `Prelude.seq`
            Prelude.rnf position `Prelude.seq`
              Prelude.rnf accessPointArn

instance Data.ToHeaders ConfigureAccessPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfigureAccessPoint where
  toJSON ConfigureAccessPoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cpiSecretKey" Data..=) Prelude.<$> cpiSecretKey,
            ("cpiUserId" Data..=) Prelude.<$> cpiUserId,
            ("cpiUserPassword" Data..=)
              Prelude.<$> cpiUserPassword,
            ("cpiUsername" Data..=) Prelude.<$> cpiUsername,
            ("position" Data..=) Prelude.<$> position,
            Prelude.Just
              ("accessPointArn" Data..= accessPointArn)
          ]
      )

instance Data.ToPath ConfigureAccessPoint where
  toPath =
    Prelude.const "/v1/network-resources/configure"

instance Data.ToQuery ConfigureAccessPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfigureAccessPointResponse' smart constructor.
data ConfigureAccessPointResponse = ConfigureAccessPointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the network resource.
    accessPoint :: NetworkResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigureAccessPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'configureAccessPointResponse_httpStatus' - The response's http status code.
--
-- 'accessPoint', 'configureAccessPointResponse_accessPoint' - Information about the network resource.
newConfigureAccessPointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessPoint'
  NetworkResource ->
  ConfigureAccessPointResponse
newConfigureAccessPointResponse
  pHttpStatus_
  pAccessPoint_ =
    ConfigureAccessPointResponse'
      { httpStatus =
          pHttpStatus_,
        accessPoint = pAccessPoint_
      }

-- | The response's http status code.
configureAccessPointResponse_httpStatus :: Lens.Lens' ConfigureAccessPointResponse Prelude.Int
configureAccessPointResponse_httpStatus = Lens.lens (\ConfigureAccessPointResponse' {httpStatus} -> httpStatus) (\s@ConfigureAccessPointResponse' {} a -> s {httpStatus = a} :: ConfigureAccessPointResponse)

-- | Information about the network resource.
configureAccessPointResponse_accessPoint :: Lens.Lens' ConfigureAccessPointResponse NetworkResource
configureAccessPointResponse_accessPoint = Lens.lens (\ConfigureAccessPointResponse' {accessPoint} -> accessPoint) (\s@ConfigureAccessPointResponse' {} a -> s {accessPoint = a} :: ConfigureAccessPointResponse)

instance Prelude.NFData ConfigureAccessPointResponse where
  rnf ConfigureAccessPointResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf accessPoint
