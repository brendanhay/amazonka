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
-- Module      : Amazonka.WorkSpacesWeb.AssociateIpAccessSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IP access settings resource with a web portal.
module Amazonka.WorkSpacesWeb.AssociateIpAccessSettings
  ( -- * Creating a Request
    AssociateIpAccessSettings (..),
    newAssociateIpAccessSettings,

    -- * Request Lenses
    associateIpAccessSettings_ipAccessSettingsArn,
    associateIpAccessSettings_portalArn,

    -- * Destructuring the Response
    AssociateIpAccessSettingsResponse (..),
    newAssociateIpAccessSettingsResponse,

    -- * Response Lenses
    associateIpAccessSettingsResponse_httpStatus,
    associateIpAccessSettingsResponse_ipAccessSettingsArn,
    associateIpAccessSettingsResponse_portalArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newAssociateIpAccessSettings' smart constructor.
data AssociateIpAccessSettings = AssociateIpAccessSettings'
  { -- | The ARN of the IP access settings.
    ipAccessSettingsArn :: Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIpAccessSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAccessSettingsArn', 'associateIpAccessSettings_ipAccessSettingsArn' - The ARN of the IP access settings.
--
-- 'portalArn', 'associateIpAccessSettings_portalArn' - The ARN of the web portal.
newAssociateIpAccessSettings ::
  -- | 'ipAccessSettingsArn'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  AssociateIpAccessSettings
newAssociateIpAccessSettings
  pIpAccessSettingsArn_
  pPortalArn_ =
    AssociateIpAccessSettings'
      { ipAccessSettingsArn =
          pIpAccessSettingsArn_,
        portalArn = pPortalArn_
      }

-- | The ARN of the IP access settings.
associateIpAccessSettings_ipAccessSettingsArn :: Lens.Lens' AssociateIpAccessSettings Prelude.Text
associateIpAccessSettings_ipAccessSettingsArn = Lens.lens (\AssociateIpAccessSettings' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@AssociateIpAccessSettings' {} a -> s {ipAccessSettingsArn = a} :: AssociateIpAccessSettings)

-- | The ARN of the web portal.
associateIpAccessSettings_portalArn :: Lens.Lens' AssociateIpAccessSettings Prelude.Text
associateIpAccessSettings_portalArn = Lens.lens (\AssociateIpAccessSettings' {portalArn} -> portalArn) (\s@AssociateIpAccessSettings' {} a -> s {portalArn = a} :: AssociateIpAccessSettings)

instance Core.AWSRequest AssociateIpAccessSettings where
  type
    AWSResponse AssociateIpAccessSettings =
      AssociateIpAccessSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateIpAccessSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ipAccessSettingsArn")
            Prelude.<*> (x Data..:> "portalArn")
      )

instance Prelude.Hashable AssociateIpAccessSettings where
  hashWithSalt _salt AssociateIpAccessSettings' {..} =
    _salt
      `Prelude.hashWithSalt` ipAccessSettingsArn
      `Prelude.hashWithSalt` portalArn

instance Prelude.NFData AssociateIpAccessSettings where
  rnf AssociateIpAccessSettings' {..} =
    Prelude.rnf ipAccessSettingsArn
      `Prelude.seq` Prelude.rnf portalArn

instance Data.ToHeaders AssociateIpAccessSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateIpAccessSettings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateIpAccessSettings where
  toPath AssociateIpAccessSettings' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/ipAccessSettings"
      ]

instance Data.ToQuery AssociateIpAccessSettings where
  toQuery AssociateIpAccessSettings' {..} =
    Prelude.mconcat
      ["ipAccessSettingsArn" Data.=: ipAccessSettingsArn]

-- | /See:/ 'newAssociateIpAccessSettingsResponse' smart constructor.
data AssociateIpAccessSettingsResponse = AssociateIpAccessSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the IP access settings resource.
    ipAccessSettingsArn :: Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIpAccessSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateIpAccessSettingsResponse_httpStatus' - The response's http status code.
--
-- 'ipAccessSettingsArn', 'associateIpAccessSettingsResponse_ipAccessSettingsArn' - The ARN of the IP access settings resource.
--
-- 'portalArn', 'associateIpAccessSettingsResponse_portalArn' - The ARN of the web portal.
newAssociateIpAccessSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'ipAccessSettingsArn'
  Prelude.Text ->
  -- | 'portalArn'
  Prelude.Text ->
  AssociateIpAccessSettingsResponse
newAssociateIpAccessSettingsResponse
  pHttpStatus_
  pIpAccessSettingsArn_
  pPortalArn_ =
    AssociateIpAccessSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        ipAccessSettingsArn =
          pIpAccessSettingsArn_,
        portalArn = pPortalArn_
      }

-- | The response's http status code.
associateIpAccessSettingsResponse_httpStatus :: Lens.Lens' AssociateIpAccessSettingsResponse Prelude.Int
associateIpAccessSettingsResponse_httpStatus = Lens.lens (\AssociateIpAccessSettingsResponse' {httpStatus} -> httpStatus) (\s@AssociateIpAccessSettingsResponse' {} a -> s {httpStatus = a} :: AssociateIpAccessSettingsResponse)

-- | The ARN of the IP access settings resource.
associateIpAccessSettingsResponse_ipAccessSettingsArn :: Lens.Lens' AssociateIpAccessSettingsResponse Prelude.Text
associateIpAccessSettingsResponse_ipAccessSettingsArn = Lens.lens (\AssociateIpAccessSettingsResponse' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@AssociateIpAccessSettingsResponse' {} a -> s {ipAccessSettingsArn = a} :: AssociateIpAccessSettingsResponse)

-- | The ARN of the web portal.
associateIpAccessSettingsResponse_portalArn :: Lens.Lens' AssociateIpAccessSettingsResponse Prelude.Text
associateIpAccessSettingsResponse_portalArn = Lens.lens (\AssociateIpAccessSettingsResponse' {portalArn} -> portalArn) (\s@AssociateIpAccessSettingsResponse' {} a -> s {portalArn = a} :: AssociateIpAccessSettingsResponse)

instance
  Prelude.NFData
    AssociateIpAccessSettingsResponse
  where
  rnf AssociateIpAccessSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ipAccessSettingsArn
      `Prelude.seq` Prelude.rnf portalArn
