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
-- Module      : Amazonka.NetworkManager.AssociateLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a link to a device. A device can be associated to multiple
-- links and a link can be associated to multiple devices. The device and
-- link must be in the same global network and the same site.
module Amazonka.NetworkManager.AssociateLink
  ( -- * Creating a Request
    AssociateLink (..),
    newAssociateLink,

    -- * Request Lenses
    associateLink_globalNetworkId,
    associateLink_deviceId,
    associateLink_linkId,

    -- * Destructuring the Response
    AssociateLinkResponse (..),
    newAssociateLinkResponse,

    -- * Response Lenses
    associateLinkResponse_linkAssociation,
    associateLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateLink' smart constructor.
data AssociateLink = AssociateLink'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'associateLink_globalNetworkId' - The ID of the global network.
--
-- 'deviceId', 'associateLink_deviceId' - The ID of the device.
--
-- 'linkId', 'associateLink_linkId' - The ID of the link.
newAssociateLink ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'linkId'
  Prelude.Text ->
  AssociateLink
newAssociateLink
  pGlobalNetworkId_
  pDeviceId_
  pLinkId_ =
    AssociateLink'
      { globalNetworkId = pGlobalNetworkId_,
        deviceId = pDeviceId_,
        linkId = pLinkId_
      }

-- | The ID of the global network.
associateLink_globalNetworkId :: Lens.Lens' AssociateLink Prelude.Text
associateLink_globalNetworkId = Lens.lens (\AssociateLink' {globalNetworkId} -> globalNetworkId) (\s@AssociateLink' {} a -> s {globalNetworkId = a} :: AssociateLink)

-- | The ID of the device.
associateLink_deviceId :: Lens.Lens' AssociateLink Prelude.Text
associateLink_deviceId = Lens.lens (\AssociateLink' {deviceId} -> deviceId) (\s@AssociateLink' {} a -> s {deviceId = a} :: AssociateLink)

-- | The ID of the link.
associateLink_linkId :: Lens.Lens' AssociateLink Prelude.Text
associateLink_linkId = Lens.lens (\AssociateLink' {linkId} -> linkId) (\s@AssociateLink' {} a -> s {linkId = a} :: AssociateLink)

instance Core.AWSRequest AssociateLink where
  type
    AWSResponse AssociateLink =
      AssociateLinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateLinkResponse'
            Prelude.<$> (x Data..?> "LinkAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateLink where
  hashWithSalt _salt AssociateLink' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` linkId

instance Prelude.NFData AssociateLink where
  rnf AssociateLink' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf linkId

instance Data.ToHeaders AssociateLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateLink where
  toJSON AssociateLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DeviceId" Data..= deviceId),
            Prelude.Just ("LinkId" Data..= linkId)
          ]
      )

instance Data.ToPath AssociateLink where
  toPath AssociateLink' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/link-associations"
      ]

instance Data.ToQuery AssociateLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateLinkResponse' smart constructor.
data AssociateLinkResponse = AssociateLinkResponse'
  { -- | The link association.
    linkAssociation :: Prelude.Maybe LinkAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkAssociation', 'associateLinkResponse_linkAssociation' - The link association.
--
-- 'httpStatus', 'associateLinkResponse_httpStatus' - The response's http status code.
newAssociateLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateLinkResponse
newAssociateLinkResponse pHttpStatus_ =
  AssociateLinkResponse'
    { linkAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The link association.
associateLinkResponse_linkAssociation :: Lens.Lens' AssociateLinkResponse (Prelude.Maybe LinkAssociation)
associateLinkResponse_linkAssociation = Lens.lens (\AssociateLinkResponse' {linkAssociation} -> linkAssociation) (\s@AssociateLinkResponse' {} a -> s {linkAssociation = a} :: AssociateLinkResponse)

-- | The response's http status code.
associateLinkResponse_httpStatus :: Lens.Lens' AssociateLinkResponse Prelude.Int
associateLinkResponse_httpStatus = Lens.lens (\AssociateLinkResponse' {httpStatus} -> httpStatus) (\s@AssociateLinkResponse' {} a -> s {httpStatus = a} :: AssociateLinkResponse)

instance Prelude.NFData AssociateLinkResponse where
  rnf AssociateLinkResponse' {..} =
    Prelude.rnf linkAssociation
      `Prelude.seq` Prelude.rnf httpStatus
