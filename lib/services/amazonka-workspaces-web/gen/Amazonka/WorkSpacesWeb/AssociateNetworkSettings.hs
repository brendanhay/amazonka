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
-- Module      : Amazonka.WorkSpacesWeb.AssociateNetworkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a network settings resource with a web portal.
module Amazonka.WorkSpacesWeb.AssociateNetworkSettings
  ( -- * Creating a Request
    AssociateNetworkSettings (..),
    newAssociateNetworkSettings,

    -- * Request Lenses
    associateNetworkSettings_networkSettingsArn,
    associateNetworkSettings_portalArn,

    -- * Destructuring the Response
    AssociateNetworkSettingsResponse (..),
    newAssociateNetworkSettingsResponse,

    -- * Response Lenses
    associateNetworkSettingsResponse_httpStatus,
    associateNetworkSettingsResponse_networkSettingsArn,
    associateNetworkSettingsResponse_portalArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newAssociateNetworkSettings' smart constructor.
data AssociateNetworkSettings = AssociateNetworkSettings'
  { -- | The ARN of the network settings.
    networkSettingsArn :: Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSettingsArn', 'associateNetworkSettings_networkSettingsArn' - The ARN of the network settings.
--
-- 'portalArn', 'associateNetworkSettings_portalArn' - The ARN of the web portal.
newAssociateNetworkSettings ::
  -- | 'networkSettingsArn'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  AssociateNetworkSettings
newAssociateNetworkSettings
  pNetworkSettingsArn_
  pPortalArn_ =
    AssociateNetworkSettings'
      { networkSettingsArn =
          pNetworkSettingsArn_,
        portalArn = pPortalArn_
      }

-- | The ARN of the network settings.
associateNetworkSettings_networkSettingsArn :: Lens.Lens' AssociateNetworkSettings Prelude.Text
associateNetworkSettings_networkSettingsArn = Lens.lens (\AssociateNetworkSettings' {networkSettingsArn} -> networkSettingsArn) (\s@AssociateNetworkSettings' {} a -> s {networkSettingsArn = a} :: AssociateNetworkSettings)

-- | The ARN of the web portal.
associateNetworkSettings_portalArn :: Lens.Lens' AssociateNetworkSettings Prelude.Text
associateNetworkSettings_portalArn = Lens.lens (\AssociateNetworkSettings' {portalArn} -> portalArn) (\s@AssociateNetworkSettings' {} a -> s {portalArn = a} :: AssociateNetworkSettings)

instance Core.AWSRequest AssociateNetworkSettings where
  type
    AWSResponse AssociateNetworkSettings =
      AssociateNetworkSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateNetworkSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "networkSettingsArn")
            Prelude.<*> (x Core..:> "portalArn")
      )

instance Prelude.Hashable AssociateNetworkSettings where
  hashWithSalt _salt AssociateNetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` networkSettingsArn
      `Prelude.hashWithSalt` portalArn

instance Prelude.NFData AssociateNetworkSettings where
  rnf AssociateNetworkSettings' {..} =
    Prelude.rnf networkSettingsArn
      `Prelude.seq` Prelude.rnf portalArn

instance Core.ToHeaders AssociateNetworkSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateNetworkSettings where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath AssociateNetworkSettings where
  toPath AssociateNetworkSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Core.toBS portalArn,
        "/networkSettings"
      ]

instance Core.ToQuery AssociateNetworkSettings where
  toQuery AssociateNetworkSettings' {..} =
    Prelude.mconcat
      ["networkSettingsArn" Core.=: networkSettingsArn]

-- | /See:/ 'newAssociateNetworkSettingsResponse' smart constructor.
data AssociateNetworkSettingsResponse = AssociateNetworkSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the network settings.
    networkSettingsArn :: Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateNetworkSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateNetworkSettingsResponse_httpStatus' - The response's http status code.
--
-- 'networkSettingsArn', 'associateNetworkSettingsResponse_networkSettingsArn' - The ARN of the network settings.
--
-- 'portalArn', 'associateNetworkSettingsResponse_portalArn' - The ARN of the web portal.
newAssociateNetworkSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'networkSettingsArn'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  AssociateNetworkSettingsResponse
newAssociateNetworkSettingsResponse
  pHttpStatus_
  pNetworkSettingsArn_
  pPortalArn_ =
    AssociateNetworkSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        networkSettingsArn = pNetworkSettingsArn_,
        portalArn = pPortalArn_
      }

-- | The response's http status code.
associateNetworkSettingsResponse_httpStatus :: Lens.Lens' AssociateNetworkSettingsResponse Prelude.Int
associateNetworkSettingsResponse_httpStatus = Lens.lens (\AssociateNetworkSettingsResponse' {httpStatus} -> httpStatus) (\s@AssociateNetworkSettingsResponse' {} a -> s {httpStatus = a} :: AssociateNetworkSettingsResponse)

-- | The ARN of the network settings.
associateNetworkSettingsResponse_networkSettingsArn :: Lens.Lens' AssociateNetworkSettingsResponse Prelude.Text
associateNetworkSettingsResponse_networkSettingsArn = Lens.lens (\AssociateNetworkSettingsResponse' {networkSettingsArn} -> networkSettingsArn) (\s@AssociateNetworkSettingsResponse' {} a -> s {networkSettingsArn = a} :: AssociateNetworkSettingsResponse)

-- | The ARN of the web portal.
associateNetworkSettingsResponse_portalArn :: Lens.Lens' AssociateNetworkSettingsResponse Prelude.Text
associateNetworkSettingsResponse_portalArn = Lens.lens (\AssociateNetworkSettingsResponse' {portalArn} -> portalArn) (\s@AssociateNetworkSettingsResponse' {} a -> s {portalArn = a} :: AssociateNetworkSettingsResponse)

instance
  Prelude.NFData
    AssociateNetworkSettingsResponse
  where
  rnf AssociateNetworkSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf networkSettingsArn
      `Prelude.seq` Prelude.rnf portalArn
