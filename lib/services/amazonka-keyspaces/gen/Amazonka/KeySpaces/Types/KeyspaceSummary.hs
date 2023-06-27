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
-- Module      : Amazonka.KeySpaces.Types.KeyspaceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.KeyspaceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.Rs
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a keyspace.
--
-- /See:/ 'newKeyspaceSummary' smart constructor.
data KeyspaceSummary = KeyspaceSummary'
  { -- | If the @replicationStrategy@ of the keyspace is @MULTI_REGION@, a list
    -- of replication Regions is returned.
    replicationRegions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the keyspace.
    keyspaceName :: Prelude.Text,
    -- | The unique identifier of the keyspace in the format of an Amazon
    -- Resource Name (ARN).
    resourceArn :: Prelude.Text,
    -- | This property specifies if a keyspace is a single Region keyspace or a
    -- multi-Region keyspace. The available values are @SINGLE_REGION@ or
    -- @MULTI_REGION@.
    replicationStrategy :: Rs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyspaceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationRegions', 'keyspaceSummary_replicationRegions' - If the @replicationStrategy@ of the keyspace is @MULTI_REGION@, a list
-- of replication Regions is returned.
--
-- 'keyspaceName', 'keyspaceSummary_keyspaceName' - The name of the keyspace.
--
-- 'resourceArn', 'keyspaceSummary_resourceArn' - The unique identifier of the keyspace in the format of an Amazon
-- Resource Name (ARN).
--
-- 'replicationStrategy', 'keyspaceSummary_replicationStrategy' - This property specifies if a keyspace is a single Region keyspace or a
-- multi-Region keyspace. The available values are @SINGLE_REGION@ or
-- @MULTI_REGION@.
newKeyspaceSummary ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'replicationStrategy'
  Rs ->
  KeyspaceSummary
newKeyspaceSummary
  pKeyspaceName_
  pResourceArn_
  pReplicationStrategy_ =
    KeyspaceSummary'
      { replicationRegions =
          Prelude.Nothing,
        keyspaceName = pKeyspaceName_,
        resourceArn = pResourceArn_,
        replicationStrategy = pReplicationStrategy_
      }

-- | If the @replicationStrategy@ of the keyspace is @MULTI_REGION@, a list
-- of replication Regions is returned.
keyspaceSummary_replicationRegions :: Lens.Lens' KeyspaceSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
keyspaceSummary_replicationRegions = Lens.lens (\KeyspaceSummary' {replicationRegions} -> replicationRegions) (\s@KeyspaceSummary' {} a -> s {replicationRegions = a} :: KeyspaceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the keyspace.
keyspaceSummary_keyspaceName :: Lens.Lens' KeyspaceSummary Prelude.Text
keyspaceSummary_keyspaceName = Lens.lens (\KeyspaceSummary' {keyspaceName} -> keyspaceName) (\s@KeyspaceSummary' {} a -> s {keyspaceName = a} :: KeyspaceSummary)

-- | The unique identifier of the keyspace in the format of an Amazon
-- Resource Name (ARN).
keyspaceSummary_resourceArn :: Lens.Lens' KeyspaceSummary Prelude.Text
keyspaceSummary_resourceArn = Lens.lens (\KeyspaceSummary' {resourceArn} -> resourceArn) (\s@KeyspaceSummary' {} a -> s {resourceArn = a} :: KeyspaceSummary)

-- | This property specifies if a keyspace is a single Region keyspace or a
-- multi-Region keyspace. The available values are @SINGLE_REGION@ or
-- @MULTI_REGION@.
keyspaceSummary_replicationStrategy :: Lens.Lens' KeyspaceSummary Rs
keyspaceSummary_replicationStrategy = Lens.lens (\KeyspaceSummary' {replicationStrategy} -> replicationStrategy) (\s@KeyspaceSummary' {} a -> s {replicationStrategy = a} :: KeyspaceSummary)

instance Data.FromJSON KeyspaceSummary where
  parseJSON =
    Data.withObject
      "KeyspaceSummary"
      ( \x ->
          KeyspaceSummary'
            Prelude.<$> (x Data..:? "replicationRegions")
            Prelude.<*> (x Data..: "keyspaceName")
            Prelude.<*> (x Data..: "resourceArn")
            Prelude.<*> (x Data..: "replicationStrategy")
      )

instance Prelude.Hashable KeyspaceSummary where
  hashWithSalt _salt KeyspaceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` replicationRegions
      `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` replicationStrategy

instance Prelude.NFData KeyspaceSummary where
  rnf KeyspaceSummary' {..} =
    Prelude.rnf replicationRegions
      `Prelude.seq` Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf replicationStrategy
