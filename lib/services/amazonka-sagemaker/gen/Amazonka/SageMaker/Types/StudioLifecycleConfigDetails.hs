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
-- Module      : Amazonka.SageMaker.Types.StudioLifecycleConfigDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.StudioLifecycleConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.StudioLifecycleConfigAppType

-- | Details of the Studio Lifecycle Configuration.
--
-- /See:/ 'newStudioLifecycleConfigDetails' smart constructor.
data StudioLifecycleConfigDetails = StudioLifecycleConfigDetails'
  { -- | The name of the Studio Lifecycle Configuration.
    studioLifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Lifecycle Configuration.
    studioLifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The App type to which the Lifecycle Configuration is attached.
    studioLifecycleConfigAppType :: Prelude.Maybe StudioLifecycleConfigAppType,
    -- | This value is equivalent to CreationTime because Studio Lifecycle
    -- Configurations are immutable.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The creation time of the Studio Lifecycle Configuration.
    creationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioLifecycleConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioLifecycleConfigName', 'studioLifecycleConfigDetails_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration.
--
-- 'studioLifecycleConfigArn', 'studioLifecycleConfigDetails_studioLifecycleConfigArn' - The Amazon Resource Name (ARN) of the Lifecycle Configuration.
--
-- 'studioLifecycleConfigAppType', 'studioLifecycleConfigDetails_studioLifecycleConfigAppType' - The App type to which the Lifecycle Configuration is attached.
--
-- 'lastModifiedTime', 'studioLifecycleConfigDetails_lastModifiedTime' - This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
--
-- 'creationTime', 'studioLifecycleConfigDetails_creationTime' - The creation time of the Studio Lifecycle Configuration.
newStudioLifecycleConfigDetails ::
  StudioLifecycleConfigDetails
newStudioLifecycleConfigDetails =
  StudioLifecycleConfigDetails'
    { studioLifecycleConfigName =
        Prelude.Nothing,
      studioLifecycleConfigArn = Prelude.Nothing,
      studioLifecycleConfigAppType =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The name of the Studio Lifecycle Configuration.
studioLifecycleConfigDetails_studioLifecycleConfigName :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.Text)
studioLifecycleConfigDetails_studioLifecycleConfigName = Lens.lens (\StudioLifecycleConfigDetails' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@StudioLifecycleConfigDetails' {} a -> s {studioLifecycleConfigName = a} :: StudioLifecycleConfigDetails)

-- | The Amazon Resource Name (ARN) of the Lifecycle Configuration.
studioLifecycleConfigDetails_studioLifecycleConfigArn :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.Text)
studioLifecycleConfigDetails_studioLifecycleConfigArn = Lens.lens (\StudioLifecycleConfigDetails' {studioLifecycleConfigArn} -> studioLifecycleConfigArn) (\s@StudioLifecycleConfigDetails' {} a -> s {studioLifecycleConfigArn = a} :: StudioLifecycleConfigDetails)

-- | The App type to which the Lifecycle Configuration is attached.
studioLifecycleConfigDetails_studioLifecycleConfigAppType :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe StudioLifecycleConfigAppType)
studioLifecycleConfigDetails_studioLifecycleConfigAppType = Lens.lens (\StudioLifecycleConfigDetails' {studioLifecycleConfigAppType} -> studioLifecycleConfigAppType) (\s@StudioLifecycleConfigDetails' {} a -> s {studioLifecycleConfigAppType = a} :: StudioLifecycleConfigDetails)

-- | This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
studioLifecycleConfigDetails_lastModifiedTime :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.UTCTime)
studioLifecycleConfigDetails_lastModifiedTime = Lens.lens (\StudioLifecycleConfigDetails' {lastModifiedTime} -> lastModifiedTime) (\s@StudioLifecycleConfigDetails' {} a -> s {lastModifiedTime = a} :: StudioLifecycleConfigDetails) Prelude.. Lens.mapping Core._Time

-- | The creation time of the Studio Lifecycle Configuration.
studioLifecycleConfigDetails_creationTime :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.UTCTime)
studioLifecycleConfigDetails_creationTime = Lens.lens (\StudioLifecycleConfigDetails' {creationTime} -> creationTime) (\s@StudioLifecycleConfigDetails' {} a -> s {creationTime = a} :: StudioLifecycleConfigDetails) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON StudioLifecycleConfigDetails where
  parseJSON =
    Core.withObject
      "StudioLifecycleConfigDetails"
      ( \x ->
          StudioLifecycleConfigDetails'
            Prelude.<$> (x Core..:? "StudioLifecycleConfigName")
            Prelude.<*> (x Core..:? "StudioLifecycleConfigArn")
            Prelude.<*> (x Core..:? "StudioLifecycleConfigAppType")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreationTime")
      )

instance
  Prelude.Hashable
    StudioLifecycleConfigDetails
  where
  hashWithSalt _salt StudioLifecycleConfigDetails' {..} =
    _salt
      `Prelude.hashWithSalt` studioLifecycleConfigName
      `Prelude.hashWithSalt` studioLifecycleConfigArn
      `Prelude.hashWithSalt` studioLifecycleConfigAppType
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData StudioLifecycleConfigDetails where
  rnf StudioLifecycleConfigDetails' {..} =
    Prelude.rnf studioLifecycleConfigName
      `Prelude.seq` Prelude.rnf studioLifecycleConfigArn
      `Prelude.seq` Prelude.rnf studioLifecycleConfigAppType
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
