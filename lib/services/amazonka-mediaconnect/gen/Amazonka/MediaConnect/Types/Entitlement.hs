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
-- Module      : Amazonka.MediaConnect.Types.Entitlement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Entitlement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.EntitlementStatus
import qualified Amazonka.Prelude as Prelude

-- | The settings for a flow entitlement.
--
-- /See:/ 'newEntitlement' smart constructor.
data Entitlement = Entitlement'
  { -- | An indication of whether the entitlement is enabled.
    entitlementStatus :: Prelude.Maybe EntitlementStatus,
    -- | A description of the entitlement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption that will be used on the output that is
    -- associated with this entitlement.
    encryption :: Prelude.Maybe Encryption,
    -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the entitlement.
    entitlementArn :: Prelude.Text,
    -- | The AWS account IDs that you want to share your content with. The
    -- receiving accounts (subscribers) will be allowed to create their own
    -- flow using your content as the source.
    subscribers :: [Prelude.Text],
    -- | The name of the entitlement.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlementStatus', 'entitlement_entitlementStatus' - An indication of whether the entitlement is enabled.
--
-- 'description', 'entitlement_description' - A description of the entitlement.
--
-- 'encryption', 'entitlement_encryption' - The type of encryption that will be used on the output that is
-- associated with this entitlement.
--
-- 'dataTransferSubscriberFeePercent', 'entitlement_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'entitlementArn', 'entitlement_entitlementArn' - The ARN of the entitlement.
--
-- 'subscribers', 'entitlement_subscribers' - The AWS account IDs that you want to share your content with. The
-- receiving accounts (subscribers) will be allowed to create their own
-- flow using your content as the source.
--
-- 'name', 'entitlement_name' - The name of the entitlement.
newEntitlement ::
  -- | 'entitlementArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Entitlement
newEntitlement pEntitlementArn_ pName_ =
  Entitlement'
    { entitlementStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      encryption = Prelude.Nothing,
      dataTransferSubscriberFeePercent = Prelude.Nothing,
      entitlementArn = pEntitlementArn_,
      subscribers = Prelude.mempty,
      name = pName_
    }

-- | An indication of whether the entitlement is enabled.
entitlement_entitlementStatus :: Lens.Lens' Entitlement (Prelude.Maybe EntitlementStatus)
entitlement_entitlementStatus = Lens.lens (\Entitlement' {entitlementStatus} -> entitlementStatus) (\s@Entitlement' {} a -> s {entitlementStatus = a} :: Entitlement)

-- | A description of the entitlement.
entitlement_description :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Text)
entitlement_description = Lens.lens (\Entitlement' {description} -> description) (\s@Entitlement' {} a -> s {description = a} :: Entitlement)

-- | The type of encryption that will be used on the output that is
-- associated with this entitlement.
entitlement_encryption :: Lens.Lens' Entitlement (Prelude.Maybe Encryption)
entitlement_encryption = Lens.lens (\Entitlement' {encryption} -> encryption) (\s@Entitlement' {} a -> s {encryption = a} :: Entitlement)

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
entitlement_dataTransferSubscriberFeePercent :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Int)
entitlement_dataTransferSubscriberFeePercent = Lens.lens (\Entitlement' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@Entitlement' {} a -> s {dataTransferSubscriberFeePercent = a} :: Entitlement)

-- | The ARN of the entitlement.
entitlement_entitlementArn :: Lens.Lens' Entitlement Prelude.Text
entitlement_entitlementArn = Lens.lens (\Entitlement' {entitlementArn} -> entitlementArn) (\s@Entitlement' {} a -> s {entitlementArn = a} :: Entitlement)

-- | The AWS account IDs that you want to share your content with. The
-- receiving accounts (subscribers) will be allowed to create their own
-- flow using your content as the source.
entitlement_subscribers :: Lens.Lens' Entitlement [Prelude.Text]
entitlement_subscribers = Lens.lens (\Entitlement' {subscribers} -> subscribers) (\s@Entitlement' {} a -> s {subscribers = a} :: Entitlement) Prelude.. Lens.coerced

-- | The name of the entitlement.
entitlement_name :: Lens.Lens' Entitlement Prelude.Text
entitlement_name = Lens.lens (\Entitlement' {name} -> name) (\s@Entitlement' {} a -> s {name = a} :: Entitlement)

instance Core.FromJSON Entitlement where
  parseJSON =
    Core.withObject
      "Entitlement"
      ( \x ->
          Entitlement'
            Prelude.<$> (x Core..:? "entitlementStatus")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "dataTransferSubscriberFeePercent")
            Prelude.<*> (x Core..: "entitlementArn")
            Prelude.<*> (x Core..:? "subscribers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Entitlement where
  hashWithSalt _salt Entitlement' {..} =
    _salt `Prelude.hashWithSalt` entitlementStatus
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` dataTransferSubscriberFeePercent
      `Prelude.hashWithSalt` entitlementArn
      `Prelude.hashWithSalt` subscribers
      `Prelude.hashWithSalt` name

instance Prelude.NFData Entitlement where
  rnf Entitlement' {..} =
    Prelude.rnf entitlementStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf dataTransferSubscriberFeePercent
      `Prelude.seq` Prelude.rnf entitlementArn
      `Prelude.seq` Prelude.rnf subscribers
      `Prelude.seq` Prelude.rnf name
