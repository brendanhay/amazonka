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
-- Module      : Amazonka.MediaConnect.Types.ListedEntitlement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.ListedEntitlement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An entitlement that has been granted to you from other AWS accounts.
--
-- /See:/ 'newListedEntitlement' smart constructor.
data ListedEntitlement = ListedEntitlement'
  { -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the entitlement.
    entitlementArn :: Prelude.Text,
    -- | The name of the entitlement.
    entitlementName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedEntitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTransferSubscriberFeePercent', 'listedEntitlement_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'entitlementArn', 'listedEntitlement_entitlementArn' - The ARN of the entitlement.
--
-- 'entitlementName', 'listedEntitlement_entitlementName' - The name of the entitlement.
newListedEntitlement ::
  -- | 'entitlementArn'
  Prelude.Text ->
  -- | 'entitlementName'
  Prelude.Text ->
  ListedEntitlement
newListedEntitlement
  pEntitlementArn_
  pEntitlementName_ =
    ListedEntitlement'
      { dataTransferSubscriberFeePercent =
          Prelude.Nothing,
        entitlementArn = pEntitlementArn_,
        entitlementName = pEntitlementName_
      }

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
listedEntitlement_dataTransferSubscriberFeePercent :: Lens.Lens' ListedEntitlement (Prelude.Maybe Prelude.Int)
listedEntitlement_dataTransferSubscriberFeePercent = Lens.lens (\ListedEntitlement' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@ListedEntitlement' {} a -> s {dataTransferSubscriberFeePercent = a} :: ListedEntitlement)

-- | The ARN of the entitlement.
listedEntitlement_entitlementArn :: Lens.Lens' ListedEntitlement Prelude.Text
listedEntitlement_entitlementArn = Lens.lens (\ListedEntitlement' {entitlementArn} -> entitlementArn) (\s@ListedEntitlement' {} a -> s {entitlementArn = a} :: ListedEntitlement)

-- | The name of the entitlement.
listedEntitlement_entitlementName :: Lens.Lens' ListedEntitlement Prelude.Text
listedEntitlement_entitlementName = Lens.lens (\ListedEntitlement' {entitlementName} -> entitlementName) (\s@ListedEntitlement' {} a -> s {entitlementName = a} :: ListedEntitlement)

instance Data.FromJSON ListedEntitlement where
  parseJSON =
    Data.withObject
      "ListedEntitlement"
      ( \x ->
          ListedEntitlement'
            Prelude.<$> (x Data..:? "dataTransferSubscriberFeePercent")
            Prelude.<*> (x Data..: "entitlementArn")
            Prelude.<*> (x Data..: "entitlementName")
      )

instance Prelude.Hashable ListedEntitlement where
  hashWithSalt _salt ListedEntitlement' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferSubscriberFeePercent
      `Prelude.hashWithSalt` entitlementArn
      `Prelude.hashWithSalt` entitlementName

instance Prelude.NFData ListedEntitlement where
  rnf ListedEntitlement' {..} =
    Prelude.rnf dataTransferSubscriberFeePercent
      `Prelude.seq` Prelude.rnf entitlementArn
      `Prelude.seq` Prelude.rnf entitlementName
