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
-- Module      : Amazonka.RolesAnywhere.Types.TrustAnchorDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.TrustAnchorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.NotificationSettingDetail
import Amazonka.RolesAnywhere.Types.Source

-- | The state of the trust anchor after a read or write operation.
--
-- /See:/ 'newTrustAnchorDetail' smart constructor.
data TrustAnchorDetail = TrustAnchorDetail'
  { -- | The ISO-8601 timestamp when the trust anchor was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether the trust anchor is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the trust anchor.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of notification settings to be associated to the trust anchor.
    notificationSettings :: Prelude.Maybe [NotificationSettingDetail],
    -- | The trust anchor type and its related certificate data.
    source :: Prelude.Maybe Source,
    -- | The ARN of the trust anchor.
    trustAnchorArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 timestamp when the trust anchor was last updated.
    updatedAt :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustAnchorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'trustAnchorDetail_createdAt' - The ISO-8601 timestamp when the trust anchor was created.
--
-- 'enabled', 'trustAnchorDetail_enabled' - Indicates whether the trust anchor is enabled.
--
-- 'name', 'trustAnchorDetail_name' - The name of the trust anchor.
--
-- 'notificationSettings', 'trustAnchorDetail_notificationSettings' - A list of notification settings to be associated to the trust anchor.
--
-- 'source', 'trustAnchorDetail_source' - The trust anchor type and its related certificate data.
--
-- 'trustAnchorArn', 'trustAnchorDetail_trustAnchorArn' - The ARN of the trust anchor.
--
-- 'trustAnchorId', 'trustAnchorDetail_trustAnchorId' - The unique identifier of the trust anchor.
--
-- 'updatedAt', 'trustAnchorDetail_updatedAt' - The ISO-8601 timestamp when the trust anchor was last updated.
newTrustAnchorDetail ::
  TrustAnchorDetail
newTrustAnchorDetail =
  TrustAnchorDetail'
    { createdAt = Prelude.Nothing,
      enabled = Prelude.Nothing,
      name = Prelude.Nothing,
      notificationSettings = Prelude.Nothing,
      source = Prelude.Nothing,
      trustAnchorArn = Prelude.Nothing,
      trustAnchorId = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The ISO-8601 timestamp when the trust anchor was created.
trustAnchorDetail_createdAt :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.UTCTime)
trustAnchorDetail_createdAt = Lens.lens (\TrustAnchorDetail' {createdAt} -> createdAt) (\s@TrustAnchorDetail' {} a -> s {createdAt = a} :: TrustAnchorDetail) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the trust anchor is enabled.
trustAnchorDetail_enabled :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Bool)
trustAnchorDetail_enabled = Lens.lens (\TrustAnchorDetail' {enabled} -> enabled) (\s@TrustAnchorDetail' {} a -> s {enabled = a} :: TrustAnchorDetail)

-- | The name of the trust anchor.
trustAnchorDetail_name :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Text)
trustAnchorDetail_name = Lens.lens (\TrustAnchorDetail' {name} -> name) (\s@TrustAnchorDetail' {} a -> s {name = a} :: TrustAnchorDetail)

-- | A list of notification settings to be associated to the trust anchor.
trustAnchorDetail_notificationSettings :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe [NotificationSettingDetail])
trustAnchorDetail_notificationSettings = Lens.lens (\TrustAnchorDetail' {notificationSettings} -> notificationSettings) (\s@TrustAnchorDetail' {} a -> s {notificationSettings = a} :: TrustAnchorDetail) Prelude.. Lens.mapping Lens.coerced

-- | The trust anchor type and its related certificate data.
trustAnchorDetail_source :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Source)
trustAnchorDetail_source = Lens.lens (\TrustAnchorDetail' {source} -> source) (\s@TrustAnchorDetail' {} a -> s {source = a} :: TrustAnchorDetail)

-- | The ARN of the trust anchor.
trustAnchorDetail_trustAnchorArn :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Text)
trustAnchorDetail_trustAnchorArn = Lens.lens (\TrustAnchorDetail' {trustAnchorArn} -> trustAnchorArn) (\s@TrustAnchorDetail' {} a -> s {trustAnchorArn = a} :: TrustAnchorDetail)

-- | The unique identifier of the trust anchor.
trustAnchorDetail_trustAnchorId :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Text)
trustAnchorDetail_trustAnchorId = Lens.lens (\TrustAnchorDetail' {trustAnchorId} -> trustAnchorId) (\s@TrustAnchorDetail' {} a -> s {trustAnchorId = a} :: TrustAnchorDetail)

-- | The ISO-8601 timestamp when the trust anchor was last updated.
trustAnchorDetail_updatedAt :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.UTCTime)
trustAnchorDetail_updatedAt = Lens.lens (\TrustAnchorDetail' {updatedAt} -> updatedAt) (\s@TrustAnchorDetail' {} a -> s {updatedAt = a} :: TrustAnchorDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TrustAnchorDetail where
  parseJSON =
    Data.withObject
      "TrustAnchorDetail"
      ( \x ->
          TrustAnchorDetail'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> ( x
                            Data..:? "notificationSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "source")
            Prelude.<*> (x Data..:? "trustAnchorArn")
            Prelude.<*> (x Data..:? "trustAnchorId")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable TrustAnchorDetail where
  hashWithSalt _salt TrustAnchorDetail' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notificationSettings
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` trustAnchorArn
      `Prelude.hashWithSalt` trustAnchorId
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData TrustAnchorDetail where
  rnf TrustAnchorDetail' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notificationSettings
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf trustAnchorArn
      `Prelude.seq` Prelude.rnf trustAnchorId
      `Prelude.seq` Prelude.rnf updatedAt
