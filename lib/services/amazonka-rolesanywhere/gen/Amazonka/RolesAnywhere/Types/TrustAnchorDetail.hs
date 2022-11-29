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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.TrustAnchorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.Source

-- | The state of the trust anchor after a read or write operation.
--
-- /See:/ 'newTrustAnchorDetail' smart constructor.
data TrustAnchorDetail = TrustAnchorDetail'
  { -- | The name of the trust anchor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the trust anchor is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The trust anchor type and its related certificate data.
    source :: Prelude.Maybe Source,
    -- | The ARN of the trust anchor.
    trustAnchorArn :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 timestamp when the trust anchor was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ISO-8601 timestamp when the trust anchor was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX
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
-- 'name', 'trustAnchorDetail_name' - The name of the trust anchor.
--
-- 'trustAnchorId', 'trustAnchorDetail_trustAnchorId' - The unique identifier of the trust anchor.
--
-- 'enabled', 'trustAnchorDetail_enabled' - Indicates whether the trust anchor is enabled.
--
-- 'source', 'trustAnchorDetail_source' - The trust anchor type and its related certificate data.
--
-- 'trustAnchorArn', 'trustAnchorDetail_trustAnchorArn' - The ARN of the trust anchor.
--
-- 'createdAt', 'trustAnchorDetail_createdAt' - The ISO-8601 timestamp when the trust anchor was created.
--
-- 'updatedAt', 'trustAnchorDetail_updatedAt' - The ISO-8601 timestamp when the trust anchor was last updated.
newTrustAnchorDetail ::
  TrustAnchorDetail
newTrustAnchorDetail =
  TrustAnchorDetail'
    { name = Prelude.Nothing,
      trustAnchorId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      source = Prelude.Nothing,
      trustAnchorArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The name of the trust anchor.
trustAnchorDetail_name :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Text)
trustAnchorDetail_name = Lens.lens (\TrustAnchorDetail' {name} -> name) (\s@TrustAnchorDetail' {} a -> s {name = a} :: TrustAnchorDetail)

-- | The unique identifier of the trust anchor.
trustAnchorDetail_trustAnchorId :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Text)
trustAnchorDetail_trustAnchorId = Lens.lens (\TrustAnchorDetail' {trustAnchorId} -> trustAnchorId) (\s@TrustAnchorDetail' {} a -> s {trustAnchorId = a} :: TrustAnchorDetail)

-- | Indicates whether the trust anchor is enabled.
trustAnchorDetail_enabled :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Bool)
trustAnchorDetail_enabled = Lens.lens (\TrustAnchorDetail' {enabled} -> enabled) (\s@TrustAnchorDetail' {} a -> s {enabled = a} :: TrustAnchorDetail)

-- | The trust anchor type and its related certificate data.
trustAnchorDetail_source :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Source)
trustAnchorDetail_source = Lens.lens (\TrustAnchorDetail' {source} -> source) (\s@TrustAnchorDetail' {} a -> s {source = a} :: TrustAnchorDetail)

-- | The ARN of the trust anchor.
trustAnchorDetail_trustAnchorArn :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.Text)
trustAnchorDetail_trustAnchorArn = Lens.lens (\TrustAnchorDetail' {trustAnchorArn} -> trustAnchorArn) (\s@TrustAnchorDetail' {} a -> s {trustAnchorArn = a} :: TrustAnchorDetail)

-- | The ISO-8601 timestamp when the trust anchor was created.
trustAnchorDetail_createdAt :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.UTCTime)
trustAnchorDetail_createdAt = Lens.lens (\TrustAnchorDetail' {createdAt} -> createdAt) (\s@TrustAnchorDetail' {} a -> s {createdAt = a} :: TrustAnchorDetail) Prelude.. Lens.mapping Core._Time

-- | The ISO-8601 timestamp when the trust anchor was last updated.
trustAnchorDetail_updatedAt :: Lens.Lens' TrustAnchorDetail (Prelude.Maybe Prelude.UTCTime)
trustAnchorDetail_updatedAt = Lens.lens (\TrustAnchorDetail' {updatedAt} -> updatedAt) (\s@TrustAnchorDetail' {} a -> s {updatedAt = a} :: TrustAnchorDetail) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TrustAnchorDetail where
  parseJSON =
    Core.withObject
      "TrustAnchorDetail"
      ( \x ->
          TrustAnchorDetail'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "trustAnchorId")
            Prelude.<*> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "trustAnchorArn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable TrustAnchorDetail where
  hashWithSalt _salt TrustAnchorDetail' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` trustAnchorId
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` trustAnchorArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData TrustAnchorDetail where
  rnf TrustAnchorDetail' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf trustAnchorId
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf trustAnchorArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
