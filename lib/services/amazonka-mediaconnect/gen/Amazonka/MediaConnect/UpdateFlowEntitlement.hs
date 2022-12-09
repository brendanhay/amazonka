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
-- Module      : Amazonka.MediaConnect.UpdateFlowEntitlement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can change an entitlement\'s description, subscribers, and
-- encryption. If you change the subscribers, the service will remove the
-- outputs that are are used by the subscribers that are removed.
module Amazonka.MediaConnect.UpdateFlowEntitlement
  ( -- * Creating a Request
    UpdateFlowEntitlement (..),
    newUpdateFlowEntitlement,

    -- * Request Lenses
    updateFlowEntitlement_description,
    updateFlowEntitlement_encryption,
    updateFlowEntitlement_entitlementStatus,
    updateFlowEntitlement_subscribers,
    updateFlowEntitlement_flowArn,
    updateFlowEntitlement_entitlementArn,

    -- * Destructuring the Response
    UpdateFlowEntitlementResponse (..),
    newUpdateFlowEntitlementResponse,

    -- * Response Lenses
    updateFlowEntitlementResponse_entitlement,
    updateFlowEntitlementResponse_flowArn,
    updateFlowEntitlementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The entitlement fields that you want to update.
--
-- /See:/ 'newUpdateFlowEntitlement' smart constructor.
data UpdateFlowEntitlement = UpdateFlowEntitlement'
  { -- | A description of the entitlement. This description appears only on the
    -- AWS Elemental MediaConnect console and will not be seen by the
    -- subscriber or end user.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption that will be used on the output associated with
    -- this entitlement.
    encryption :: Prelude.Maybe UpdateEncryption,
    -- | An indication of whether you want to enable the entitlement to allow
    -- access, or disable it to stop streaming content to the subscriber’s flow
    -- temporarily. If you don’t specify the entitlementStatus field in your
    -- request, MediaConnect leaves the value unchanged.
    entitlementStatus :: Prelude.Maybe EntitlementStatus,
    -- | The AWS account IDs that you want to share your content with. The
    -- receiving accounts (subscribers) will be allowed to create their own
    -- flow using your content as the source.
    subscribers :: Prelude.Maybe [Prelude.Text],
    -- | The flow that is associated with the entitlement that you want to
    -- update.
    flowArn :: Prelude.Text,
    -- | The ARN of the entitlement that you want to update.
    entitlementArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowEntitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateFlowEntitlement_description' - A description of the entitlement. This description appears only on the
-- AWS Elemental MediaConnect console and will not be seen by the
-- subscriber or end user.
--
-- 'encryption', 'updateFlowEntitlement_encryption' - The type of encryption that will be used on the output associated with
-- this entitlement.
--
-- 'entitlementStatus', 'updateFlowEntitlement_entitlementStatus' - An indication of whether you want to enable the entitlement to allow
-- access, or disable it to stop streaming content to the subscriber’s flow
-- temporarily. If you don’t specify the entitlementStatus field in your
-- request, MediaConnect leaves the value unchanged.
--
-- 'subscribers', 'updateFlowEntitlement_subscribers' - The AWS account IDs that you want to share your content with. The
-- receiving accounts (subscribers) will be allowed to create their own
-- flow using your content as the source.
--
-- 'flowArn', 'updateFlowEntitlement_flowArn' - The flow that is associated with the entitlement that you want to
-- update.
--
-- 'entitlementArn', 'updateFlowEntitlement_entitlementArn' - The ARN of the entitlement that you want to update.
newUpdateFlowEntitlement ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'entitlementArn'
  Prelude.Text ->
  UpdateFlowEntitlement
newUpdateFlowEntitlement pFlowArn_ pEntitlementArn_ =
  UpdateFlowEntitlement'
    { description =
        Prelude.Nothing,
      encryption = Prelude.Nothing,
      entitlementStatus = Prelude.Nothing,
      subscribers = Prelude.Nothing,
      flowArn = pFlowArn_,
      entitlementArn = pEntitlementArn_
    }

-- | A description of the entitlement. This description appears only on the
-- AWS Elemental MediaConnect console and will not be seen by the
-- subscriber or end user.
updateFlowEntitlement_description :: Lens.Lens' UpdateFlowEntitlement (Prelude.Maybe Prelude.Text)
updateFlowEntitlement_description = Lens.lens (\UpdateFlowEntitlement' {description} -> description) (\s@UpdateFlowEntitlement' {} a -> s {description = a} :: UpdateFlowEntitlement)

-- | The type of encryption that will be used on the output associated with
-- this entitlement.
updateFlowEntitlement_encryption :: Lens.Lens' UpdateFlowEntitlement (Prelude.Maybe UpdateEncryption)
updateFlowEntitlement_encryption = Lens.lens (\UpdateFlowEntitlement' {encryption} -> encryption) (\s@UpdateFlowEntitlement' {} a -> s {encryption = a} :: UpdateFlowEntitlement)

-- | An indication of whether you want to enable the entitlement to allow
-- access, or disable it to stop streaming content to the subscriber’s flow
-- temporarily. If you don’t specify the entitlementStatus field in your
-- request, MediaConnect leaves the value unchanged.
updateFlowEntitlement_entitlementStatus :: Lens.Lens' UpdateFlowEntitlement (Prelude.Maybe EntitlementStatus)
updateFlowEntitlement_entitlementStatus = Lens.lens (\UpdateFlowEntitlement' {entitlementStatus} -> entitlementStatus) (\s@UpdateFlowEntitlement' {} a -> s {entitlementStatus = a} :: UpdateFlowEntitlement)

-- | The AWS account IDs that you want to share your content with. The
-- receiving accounts (subscribers) will be allowed to create their own
-- flow using your content as the source.
updateFlowEntitlement_subscribers :: Lens.Lens' UpdateFlowEntitlement (Prelude.Maybe [Prelude.Text])
updateFlowEntitlement_subscribers = Lens.lens (\UpdateFlowEntitlement' {subscribers} -> subscribers) (\s@UpdateFlowEntitlement' {} a -> s {subscribers = a} :: UpdateFlowEntitlement) Prelude.. Lens.mapping Lens.coerced

-- | The flow that is associated with the entitlement that you want to
-- update.
updateFlowEntitlement_flowArn :: Lens.Lens' UpdateFlowEntitlement Prelude.Text
updateFlowEntitlement_flowArn = Lens.lens (\UpdateFlowEntitlement' {flowArn} -> flowArn) (\s@UpdateFlowEntitlement' {} a -> s {flowArn = a} :: UpdateFlowEntitlement)

-- | The ARN of the entitlement that you want to update.
updateFlowEntitlement_entitlementArn :: Lens.Lens' UpdateFlowEntitlement Prelude.Text
updateFlowEntitlement_entitlementArn = Lens.lens (\UpdateFlowEntitlement' {entitlementArn} -> entitlementArn) (\s@UpdateFlowEntitlement' {} a -> s {entitlementArn = a} :: UpdateFlowEntitlement)

instance Core.AWSRequest UpdateFlowEntitlement where
  type
    AWSResponse UpdateFlowEntitlement =
      UpdateFlowEntitlementResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFlowEntitlementResponse'
            Prelude.<$> (x Data..?> "entitlement")
            Prelude.<*> (x Data..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFlowEntitlement where
  hashWithSalt _salt UpdateFlowEntitlement' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` entitlementStatus
      `Prelude.hashWithSalt` subscribers
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` entitlementArn

instance Prelude.NFData UpdateFlowEntitlement where
  rnf UpdateFlowEntitlement' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf entitlementStatus
      `Prelude.seq` Prelude.rnf subscribers
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf entitlementArn

instance Data.ToHeaders UpdateFlowEntitlement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFlowEntitlement where
  toJSON UpdateFlowEntitlement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("encryption" Data..=) Prelude.<$> encryption,
            ("entitlementStatus" Data..=)
              Prelude.<$> entitlementStatus,
            ("subscribers" Data..=) Prelude.<$> subscribers
          ]
      )

instance Data.ToPath UpdateFlowEntitlement where
  toPath UpdateFlowEntitlement' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Data.toBS flowArn,
        "/entitlements/",
        Data.toBS entitlementArn
      ]

instance Data.ToQuery UpdateFlowEntitlement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFlowEntitlementResponse' smart constructor.
data UpdateFlowEntitlementResponse = UpdateFlowEntitlementResponse'
  { -- | The new configuration of the entitlement that you updated.
    entitlement :: Prelude.Maybe Entitlement,
    -- | The ARN of the flow that this entitlement was granted on.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowEntitlementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlement', 'updateFlowEntitlementResponse_entitlement' - The new configuration of the entitlement that you updated.
--
-- 'flowArn', 'updateFlowEntitlementResponse_flowArn' - The ARN of the flow that this entitlement was granted on.
--
-- 'httpStatus', 'updateFlowEntitlementResponse_httpStatus' - The response's http status code.
newUpdateFlowEntitlementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFlowEntitlementResponse
newUpdateFlowEntitlementResponse pHttpStatus_ =
  UpdateFlowEntitlementResponse'
    { entitlement =
        Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new configuration of the entitlement that you updated.
updateFlowEntitlementResponse_entitlement :: Lens.Lens' UpdateFlowEntitlementResponse (Prelude.Maybe Entitlement)
updateFlowEntitlementResponse_entitlement = Lens.lens (\UpdateFlowEntitlementResponse' {entitlement} -> entitlement) (\s@UpdateFlowEntitlementResponse' {} a -> s {entitlement = a} :: UpdateFlowEntitlementResponse)

-- | The ARN of the flow that this entitlement was granted on.
updateFlowEntitlementResponse_flowArn :: Lens.Lens' UpdateFlowEntitlementResponse (Prelude.Maybe Prelude.Text)
updateFlowEntitlementResponse_flowArn = Lens.lens (\UpdateFlowEntitlementResponse' {flowArn} -> flowArn) (\s@UpdateFlowEntitlementResponse' {} a -> s {flowArn = a} :: UpdateFlowEntitlementResponse)

-- | The response's http status code.
updateFlowEntitlementResponse_httpStatus :: Lens.Lens' UpdateFlowEntitlementResponse Prelude.Int
updateFlowEntitlementResponse_httpStatus = Lens.lens (\UpdateFlowEntitlementResponse' {httpStatus} -> httpStatus) (\s@UpdateFlowEntitlementResponse' {} a -> s {httpStatus = a} :: UpdateFlowEntitlementResponse)

instance Prelude.NFData UpdateFlowEntitlementResponse where
  rnf UpdateFlowEntitlementResponse' {..} =
    Prelude.rnf entitlement
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
