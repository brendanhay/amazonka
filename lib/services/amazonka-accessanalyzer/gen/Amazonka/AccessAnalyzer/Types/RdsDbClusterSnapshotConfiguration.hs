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
-- Module      : Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotConfiguration where

import Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotAttributeValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed access control configuration for an Amazon RDS DB cluster
-- snapshot. You can propose a configuration for a new Amazon RDS DB
-- cluster snapshot or an Amazon RDS DB cluster snapshot that you own by
-- specifying the @RdsDbClusterSnapshotAttributeValue@ and optional KMS
-- encryption key. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBClusterSnapshotAttribute.html ModifyDBClusterSnapshotAttribute>.
--
-- /See:/ 'newRdsDbClusterSnapshotConfiguration' smart constructor.
data RdsDbClusterSnapshotConfiguration = RdsDbClusterSnapshotConfiguration'
  { -- | The names and values of manual DB cluster snapshot attributes. Manual DB
    -- cluster snapshot attributes are used to authorize other Amazon Web
    -- Services accounts to restore a manual DB cluster snapshot. The only
    -- valid value for @AttributeName@ for the attribute map is @restore@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text RdsDbClusterSnapshotAttributeValue),
    -- | The KMS key identifier for an encrypted Amazon RDS DB cluster snapshot.
    -- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
    -- for the KMS key.
    --
    -- -   If the configuration is for an existing Amazon RDS DB cluster
    --     snapshot and you do not specify the @kmsKeyId@, or you specify an
    --     empty string, then the access preview uses the existing @kmsKeyId@
    --     of the snapshot.
    --
    -- -   If the access preview is for a new resource and you do not specify
    --     the specify the @kmsKeyId@, then the access preview considers the
    --     snapshot as unencrypted.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsDbClusterSnapshotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'rdsDbClusterSnapshotConfiguration_attributes' - The names and values of manual DB cluster snapshot attributes. Manual DB
-- cluster snapshot attributes are used to authorize other Amazon Web
-- Services accounts to restore a manual DB cluster snapshot. The only
-- valid value for @AttributeName@ for the attribute map is @restore@
--
-- 'kmsKeyId', 'rdsDbClusterSnapshotConfiguration_kmsKeyId' - The KMS key identifier for an encrypted Amazon RDS DB cluster snapshot.
-- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
-- for the KMS key.
--
-- -   If the configuration is for an existing Amazon RDS DB cluster
--     snapshot and you do not specify the @kmsKeyId@, or you specify an
--     empty string, then the access preview uses the existing @kmsKeyId@
--     of the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the specify the @kmsKeyId@, then the access preview considers the
--     snapshot as unencrypted.
newRdsDbClusterSnapshotConfiguration ::
  RdsDbClusterSnapshotConfiguration
newRdsDbClusterSnapshotConfiguration =
  RdsDbClusterSnapshotConfiguration'
    { attributes =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | The names and values of manual DB cluster snapshot attributes. Manual DB
-- cluster snapshot attributes are used to authorize other Amazon Web
-- Services accounts to restore a manual DB cluster snapshot. The only
-- valid value for @AttributeName@ for the attribute map is @restore@
rdsDbClusterSnapshotConfiguration_attributes :: Lens.Lens' RdsDbClusterSnapshotConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text RdsDbClusterSnapshotAttributeValue))
rdsDbClusterSnapshotConfiguration_attributes = Lens.lens (\RdsDbClusterSnapshotConfiguration' {attributes} -> attributes) (\s@RdsDbClusterSnapshotConfiguration' {} a -> s {attributes = a} :: RdsDbClusterSnapshotConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key identifier for an encrypted Amazon RDS DB cluster snapshot.
-- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
-- for the KMS key.
--
-- -   If the configuration is for an existing Amazon RDS DB cluster
--     snapshot and you do not specify the @kmsKeyId@, or you specify an
--     empty string, then the access preview uses the existing @kmsKeyId@
--     of the snapshot.
--
-- -   If the access preview is for a new resource and you do not specify
--     the specify the @kmsKeyId@, then the access preview considers the
--     snapshot as unencrypted.
rdsDbClusterSnapshotConfiguration_kmsKeyId :: Lens.Lens' RdsDbClusterSnapshotConfiguration (Prelude.Maybe Prelude.Text)
rdsDbClusterSnapshotConfiguration_kmsKeyId = Lens.lens (\RdsDbClusterSnapshotConfiguration' {kmsKeyId} -> kmsKeyId) (\s@RdsDbClusterSnapshotConfiguration' {} a -> s {kmsKeyId = a} :: RdsDbClusterSnapshotConfiguration)

instance
  Data.FromJSON
    RdsDbClusterSnapshotConfiguration
  where
  parseJSON =
    Data.withObject
      "RdsDbClusterSnapshotConfiguration"
      ( \x ->
          RdsDbClusterSnapshotConfiguration'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "kmsKeyId")
      )

instance
  Prelude.Hashable
    RdsDbClusterSnapshotConfiguration
  where
  hashWithSalt
    _salt
    RdsDbClusterSnapshotConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` attributes
        `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    RdsDbClusterSnapshotConfiguration
  where
  rnf RdsDbClusterSnapshotConfiguration' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf kmsKeyId

instance
  Data.ToJSON
    RdsDbClusterSnapshotConfiguration
  where
  toJSON RdsDbClusterSnapshotConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId
          ]
      )
