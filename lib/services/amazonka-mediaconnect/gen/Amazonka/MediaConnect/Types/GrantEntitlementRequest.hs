{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.Types.GrantEntitlementRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.GrantEntitlementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.EntitlementStatus
import qualified Amazonka.Prelude as Prelude

-- | The entitlements that you want to grant on a flow.
--
-- /See:/ 'newGrantEntitlementRequest' smart constructor.
data GrantEntitlementRequest = GrantEntitlementRequest'
  { -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | A description of the entitlement. This description appears only on the
    -- AWS Elemental MediaConnect console and will not be seen by the
    -- subscriber or end user.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption that will be used on the output that is
    -- associated with this entitlement.
    encryption :: Prelude.Maybe Encryption,
    -- | An indication of whether the new entitlement should be enabled or
    -- disabled as soon as it is created. If you don’t specify the
    -- entitlementStatus field in your request, MediaConnect sets it to
    -- ENABLED.
    entitlementStatus :: Prelude.Maybe EntitlementStatus,
    -- | The name of the entitlement. This value must be unique within the
    -- current flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The AWS account IDs that you want to share your content with. The
    -- receiving accounts (subscribers) will be allowed to create their own
    -- flows using your content as the source.
    subscribers :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrantEntitlementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTransferSubscriberFeePercent', 'grantEntitlementRequest_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'description', 'grantEntitlementRequest_description' - A description of the entitlement. This description appears only on the
-- AWS Elemental MediaConnect console and will not be seen by the
-- subscriber or end user.
--
-- 'encryption', 'grantEntitlementRequest_encryption' - The type of encryption that will be used on the output that is
-- associated with this entitlement.
--
-- 'entitlementStatus', 'grantEntitlementRequest_entitlementStatus' - An indication of whether the new entitlement should be enabled or
-- disabled as soon as it is created. If you don’t specify the
-- entitlementStatus field in your request, MediaConnect sets it to
-- ENABLED.
--
-- 'name', 'grantEntitlementRequest_name' - The name of the entitlement. This value must be unique within the
-- current flow.
--
-- 'subscribers', 'grantEntitlementRequest_subscribers' - The AWS account IDs that you want to share your content with. The
-- receiving accounts (subscribers) will be allowed to create their own
-- flows using your content as the source.
newGrantEntitlementRequest ::
  GrantEntitlementRequest
newGrantEntitlementRequest =
  GrantEntitlementRequest'
    { dataTransferSubscriberFeePercent =
        Prelude.Nothing,
      description = Prelude.Nothing,
      encryption = Prelude.Nothing,
      entitlementStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      subscribers = Prelude.mempty
    }

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
grantEntitlementRequest_dataTransferSubscriberFeePercent :: Lens.Lens' GrantEntitlementRequest (Prelude.Maybe Prelude.Int)
grantEntitlementRequest_dataTransferSubscriberFeePercent = Lens.lens (\GrantEntitlementRequest' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@GrantEntitlementRequest' {} a -> s {dataTransferSubscriberFeePercent = a} :: GrantEntitlementRequest)

-- | A description of the entitlement. This description appears only on the
-- AWS Elemental MediaConnect console and will not be seen by the
-- subscriber or end user.
grantEntitlementRequest_description :: Lens.Lens' GrantEntitlementRequest (Prelude.Maybe Prelude.Text)
grantEntitlementRequest_description = Lens.lens (\GrantEntitlementRequest' {description} -> description) (\s@GrantEntitlementRequest' {} a -> s {description = a} :: GrantEntitlementRequest)

-- | The type of encryption that will be used on the output that is
-- associated with this entitlement.
grantEntitlementRequest_encryption :: Lens.Lens' GrantEntitlementRequest (Prelude.Maybe Encryption)
grantEntitlementRequest_encryption = Lens.lens (\GrantEntitlementRequest' {encryption} -> encryption) (\s@GrantEntitlementRequest' {} a -> s {encryption = a} :: GrantEntitlementRequest)

-- | An indication of whether the new entitlement should be enabled or
-- disabled as soon as it is created. If you don’t specify the
-- entitlementStatus field in your request, MediaConnect sets it to
-- ENABLED.
grantEntitlementRequest_entitlementStatus :: Lens.Lens' GrantEntitlementRequest (Prelude.Maybe EntitlementStatus)
grantEntitlementRequest_entitlementStatus = Lens.lens (\GrantEntitlementRequest' {entitlementStatus} -> entitlementStatus) (\s@GrantEntitlementRequest' {} a -> s {entitlementStatus = a} :: GrantEntitlementRequest)

-- | The name of the entitlement. This value must be unique within the
-- current flow.
grantEntitlementRequest_name :: Lens.Lens' GrantEntitlementRequest (Prelude.Maybe Prelude.Text)
grantEntitlementRequest_name = Lens.lens (\GrantEntitlementRequest' {name} -> name) (\s@GrantEntitlementRequest' {} a -> s {name = a} :: GrantEntitlementRequest)

-- | The AWS account IDs that you want to share your content with. The
-- receiving accounts (subscribers) will be allowed to create their own
-- flows using your content as the source.
grantEntitlementRequest_subscribers :: Lens.Lens' GrantEntitlementRequest [Prelude.Text]
grantEntitlementRequest_subscribers = Lens.lens (\GrantEntitlementRequest' {subscribers} -> subscribers) (\s@GrantEntitlementRequest' {} a -> s {subscribers = a} :: GrantEntitlementRequest) Prelude.. Lens.coerced

instance Prelude.Hashable GrantEntitlementRequest where
  hashWithSalt _salt GrantEntitlementRequest' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferSubscriberFeePercent
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` entitlementStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subscribers

instance Prelude.NFData GrantEntitlementRequest where
  rnf GrantEntitlementRequest' {..} =
    Prelude.rnf dataTransferSubscriberFeePercent
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf entitlementStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf subscribers

instance Data.ToJSON GrantEntitlementRequest where
  toJSON GrantEntitlementRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataTransferSubscriberFeePercent" Data..=)
              Prelude.<$> dataTransferSubscriberFeePercent,
            ("description" Data..=) Prelude.<$> description,
            ("encryption" Data..=) Prelude.<$> encryption,
            ("entitlementStatus" Data..=)
              Prelude.<$> entitlementStatus,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("subscribers" Data..= subscribers)
          ]
      )
