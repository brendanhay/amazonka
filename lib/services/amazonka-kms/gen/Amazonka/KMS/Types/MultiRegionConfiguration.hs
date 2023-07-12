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
-- Module      : Amazonka.KMS.Types.MultiRegionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.MultiRegionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.MultiRegionKey
import Amazonka.KMS.Types.MultiRegionKeyType
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of this multi-Region key. This field appears
-- only when the KMS key is a primary or replica of a multi-Region key.
--
-- For more information about any listed KMS key, use the DescribeKey
-- operation.
--
-- /See:/ 'newMultiRegionConfiguration' smart constructor.
data MultiRegionConfiguration = MultiRegionConfiguration'
  { -- | Indicates whether the KMS key is a @PRIMARY@ or @REPLICA@ key.
    multiRegionKeyType :: Prelude.Maybe MultiRegionKeyType,
    -- | Displays the key ARN and Region of the primary key. This field includes
    -- the current KMS key if it is the primary key.
    primaryKey :: Prelude.Maybe MultiRegionKey,
    -- | displays the key ARNs and Regions of all replica keys. This field
    -- includes the current KMS key if it is a replica key.
    replicaKeys :: Prelude.Maybe [MultiRegionKey]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiRegionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiRegionKeyType', 'multiRegionConfiguration_multiRegionKeyType' - Indicates whether the KMS key is a @PRIMARY@ or @REPLICA@ key.
--
-- 'primaryKey', 'multiRegionConfiguration_primaryKey' - Displays the key ARN and Region of the primary key. This field includes
-- the current KMS key if it is the primary key.
--
-- 'replicaKeys', 'multiRegionConfiguration_replicaKeys' - displays the key ARNs and Regions of all replica keys. This field
-- includes the current KMS key if it is a replica key.
newMultiRegionConfiguration ::
  MultiRegionConfiguration
newMultiRegionConfiguration =
  MultiRegionConfiguration'
    { multiRegionKeyType =
        Prelude.Nothing,
      primaryKey = Prelude.Nothing,
      replicaKeys = Prelude.Nothing
    }

-- | Indicates whether the KMS key is a @PRIMARY@ or @REPLICA@ key.
multiRegionConfiguration_multiRegionKeyType :: Lens.Lens' MultiRegionConfiguration (Prelude.Maybe MultiRegionKeyType)
multiRegionConfiguration_multiRegionKeyType = Lens.lens (\MultiRegionConfiguration' {multiRegionKeyType} -> multiRegionKeyType) (\s@MultiRegionConfiguration' {} a -> s {multiRegionKeyType = a} :: MultiRegionConfiguration)

-- | Displays the key ARN and Region of the primary key. This field includes
-- the current KMS key if it is the primary key.
multiRegionConfiguration_primaryKey :: Lens.Lens' MultiRegionConfiguration (Prelude.Maybe MultiRegionKey)
multiRegionConfiguration_primaryKey = Lens.lens (\MultiRegionConfiguration' {primaryKey} -> primaryKey) (\s@MultiRegionConfiguration' {} a -> s {primaryKey = a} :: MultiRegionConfiguration)

-- | displays the key ARNs and Regions of all replica keys. This field
-- includes the current KMS key if it is a replica key.
multiRegionConfiguration_replicaKeys :: Lens.Lens' MultiRegionConfiguration (Prelude.Maybe [MultiRegionKey])
multiRegionConfiguration_replicaKeys = Lens.lens (\MultiRegionConfiguration' {replicaKeys} -> replicaKeys) (\s@MultiRegionConfiguration' {} a -> s {replicaKeys = a} :: MultiRegionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MultiRegionConfiguration where
  parseJSON =
    Data.withObject
      "MultiRegionConfiguration"
      ( \x ->
          MultiRegionConfiguration'
            Prelude.<$> (x Data..:? "MultiRegionKeyType")
            Prelude.<*> (x Data..:? "PrimaryKey")
            Prelude.<*> (x Data..:? "ReplicaKeys" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MultiRegionConfiguration where
  hashWithSalt _salt MultiRegionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` multiRegionKeyType
      `Prelude.hashWithSalt` primaryKey
      `Prelude.hashWithSalt` replicaKeys

instance Prelude.NFData MultiRegionConfiguration where
  rnf MultiRegionConfiguration' {..} =
    Prelude.rnf multiRegionKeyType
      `Prelude.seq` Prelude.rnf primaryKey
      `Prelude.seq` Prelude.rnf replicaKeys
