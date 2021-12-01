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
-- Module      : Amazonka.Kafka.Types.CompatibleKafkaVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.CompatibleKafkaVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains source Kafka versions and compatible target Kafka versions.
--
-- /See:/ 'newCompatibleKafkaVersion' smart constructor.
data CompatibleKafkaVersion = CompatibleKafkaVersion'
  { -- | A Kafka version.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of Kafka versions.
    targetVersions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompatibleKafkaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'compatibleKafkaVersion_sourceVersion' - A Kafka version.
--
-- 'targetVersions', 'compatibleKafkaVersion_targetVersions' - A list of Kafka versions.
newCompatibleKafkaVersion ::
  CompatibleKafkaVersion
newCompatibleKafkaVersion =
  CompatibleKafkaVersion'
    { sourceVersion =
        Prelude.Nothing,
      targetVersions = Prelude.Nothing
    }

-- | A Kafka version.
compatibleKafkaVersion_sourceVersion :: Lens.Lens' CompatibleKafkaVersion (Prelude.Maybe Prelude.Text)
compatibleKafkaVersion_sourceVersion = Lens.lens (\CompatibleKafkaVersion' {sourceVersion} -> sourceVersion) (\s@CompatibleKafkaVersion' {} a -> s {sourceVersion = a} :: CompatibleKafkaVersion)

-- | A list of Kafka versions.
compatibleKafkaVersion_targetVersions :: Lens.Lens' CompatibleKafkaVersion (Prelude.Maybe [Prelude.Text])
compatibleKafkaVersion_targetVersions = Lens.lens (\CompatibleKafkaVersion' {targetVersions} -> targetVersions) (\s@CompatibleKafkaVersion' {} a -> s {targetVersions = a} :: CompatibleKafkaVersion) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CompatibleKafkaVersion where
  parseJSON =
    Core.withObject
      "CompatibleKafkaVersion"
      ( \x ->
          CompatibleKafkaVersion'
            Prelude.<$> (x Core..:? "sourceVersion")
            Prelude.<*> ( x Core..:? "targetVersions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CompatibleKafkaVersion where
  hashWithSalt salt' CompatibleKafkaVersion' {..} =
    salt' `Prelude.hashWithSalt` targetVersions
      `Prelude.hashWithSalt` sourceVersion

instance Prelude.NFData CompatibleKafkaVersion where
  rnf CompatibleKafkaVersion' {..} =
    Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf targetVersions
