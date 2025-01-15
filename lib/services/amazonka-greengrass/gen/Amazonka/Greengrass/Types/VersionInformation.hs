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
-- Module      : Amazonka.Greengrass.Types.VersionInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.VersionInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a version.
--
-- /See:/ 'newVersionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'versionInformation_arn' - The ARN of the version.
--
-- 'creationTimestamp', 'versionInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'id', 'versionInformation_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'versionInformation_version' - The ID of the version.
newVersionInformation ::
  VersionInformation
newVersionInformation =
  VersionInformation'
    { arn = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The ARN of the version.
versionInformation_arn :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_arn = Lens.lens (\VersionInformation' {arn} -> arn) (\s@VersionInformation' {} a -> s {arn = a} :: VersionInformation)

-- | The time, in milliseconds since the epoch, when the version was created.
versionInformation_creationTimestamp :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_creationTimestamp = Lens.lens (\VersionInformation' {creationTimestamp} -> creationTimestamp) (\s@VersionInformation' {} a -> s {creationTimestamp = a} :: VersionInformation)

-- | The ID of the parent definition that the version is associated with.
versionInformation_id :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_id = Lens.lens (\VersionInformation' {id} -> id) (\s@VersionInformation' {} a -> s {id = a} :: VersionInformation)

-- | The ID of the version.
versionInformation_version :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_version = Lens.lens (\VersionInformation' {version} -> version) (\s@VersionInformation' {} a -> s {version = a} :: VersionInformation)

instance Data.FromJSON VersionInformation where
  parseJSON =
    Data.withObject
      "VersionInformation"
      ( \x ->
          VersionInformation'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable VersionInformation where
  hashWithSalt _salt VersionInformation' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version

instance Prelude.NFData VersionInformation where
  rnf VersionInformation' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf version
