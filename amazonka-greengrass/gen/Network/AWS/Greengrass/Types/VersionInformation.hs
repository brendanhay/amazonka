{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.Types.VersionInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.VersionInformation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a version.
--
-- /See:/ 'newVersionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VersionInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'versionInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'versionInformation_arn' - The ARN of the version.
--
-- 'id', 'versionInformation_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'versionInformation_version' - The ID of the version.
newVersionInformation ::
  VersionInformation
newVersionInformation =
  VersionInformation'
    { creationTimestamp =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the version was created.
versionInformation_creationTimestamp :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_creationTimestamp = Lens.lens (\VersionInformation' {creationTimestamp} -> creationTimestamp) (\s@VersionInformation' {} a -> s {creationTimestamp = a} :: VersionInformation)

-- | The ARN of the version.
versionInformation_arn :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_arn = Lens.lens (\VersionInformation' {arn} -> arn) (\s@VersionInformation' {} a -> s {arn = a} :: VersionInformation)

-- | The ID of the parent definition that the version is associated with.
versionInformation_id :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_id = Lens.lens (\VersionInformation' {id} -> id) (\s@VersionInformation' {} a -> s {id = a} :: VersionInformation)

-- | The ID of the version.
versionInformation_version :: Lens.Lens' VersionInformation (Prelude.Maybe Prelude.Text)
versionInformation_version = Lens.lens (\VersionInformation' {version} -> version) (\s@VersionInformation' {} a -> s {version = a} :: VersionInformation)

instance Prelude.FromJSON VersionInformation where
  parseJSON =
    Prelude.withObject
      "VersionInformation"
      ( \x ->
          VersionInformation'
            Prelude.<$> (x Prelude..:? "CreationTimestamp")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Version")
      )

instance Prelude.Hashable VersionInformation

instance Prelude.NFData VersionInformation
