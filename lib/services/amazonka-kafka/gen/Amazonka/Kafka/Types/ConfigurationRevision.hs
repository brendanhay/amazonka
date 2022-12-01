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
-- Module      : Amazonka.Kafka.Types.ConfigurationRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ConfigurationRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a configuration revision.
--
-- /See:/ 'newConfigurationRevision' smart constructor.
data ConfigurationRevision = ConfigurationRevision'
  { -- | The description of the configuration revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | The revision number.
    revision :: Prelude.Integer,
    -- | The time when the configuration revision was created.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'configurationRevision_description' - The description of the configuration revision.
--
-- 'revision', 'configurationRevision_revision' - The revision number.
--
-- 'creationTime', 'configurationRevision_creationTime' - The time when the configuration revision was created.
newConfigurationRevision ::
  -- | 'revision'
  Prelude.Integer ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ConfigurationRevision
newConfigurationRevision pRevision_ pCreationTime_ =
  ConfigurationRevision'
    { description =
        Prelude.Nothing,
      revision = pRevision_,
      creationTime = Core._Time Lens.# pCreationTime_
    }

-- | The description of the configuration revision.
configurationRevision_description :: Lens.Lens' ConfigurationRevision (Prelude.Maybe Prelude.Text)
configurationRevision_description = Lens.lens (\ConfigurationRevision' {description} -> description) (\s@ConfigurationRevision' {} a -> s {description = a} :: ConfigurationRevision)

-- | The revision number.
configurationRevision_revision :: Lens.Lens' ConfigurationRevision Prelude.Integer
configurationRevision_revision = Lens.lens (\ConfigurationRevision' {revision} -> revision) (\s@ConfigurationRevision' {} a -> s {revision = a} :: ConfigurationRevision)

-- | The time when the configuration revision was created.
configurationRevision_creationTime :: Lens.Lens' ConfigurationRevision Prelude.UTCTime
configurationRevision_creationTime = Lens.lens (\ConfigurationRevision' {creationTime} -> creationTime) (\s@ConfigurationRevision' {} a -> s {creationTime = a} :: ConfigurationRevision) Prelude.. Core._Time

instance Core.FromJSON ConfigurationRevision where
  parseJSON =
    Core.withObject
      "ConfigurationRevision"
      ( \x ->
          ConfigurationRevision'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..: "revision")
            Prelude.<*> (x Core..: "creationTime")
      )

instance Prelude.Hashable ConfigurationRevision where
  hashWithSalt _salt ConfigurationRevision' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ConfigurationRevision where
  rnf ConfigurationRevision' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf creationTime
