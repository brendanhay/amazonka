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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.StudioLifecycleConfigAppType

-- | Details of the Studio Lifecycle Configuration.
--
-- /See:/ 'newStudioLifecycleConfigDetails' smart constructor.
data StudioLifecycleConfigDetails = StudioLifecycleConfigDetails'
  { -- | The creation time of the Studio Lifecycle Configuration.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | This value is equivalent to CreationTime because Studio Lifecycle
    -- Configurations are immutable.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The App type to which the Lifecycle Configuration is attached.
    studioLifecycleConfigAppType :: Prelude.Maybe StudioLifecycleConfigAppType,
    -- | The Amazon Resource Name (ARN) of the Lifecycle Configuration.
    studioLifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Studio Lifecycle Configuration.
    studioLifecycleConfigName :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'studioLifecycleConfigDetails_creationTime' - The creation time of the Studio Lifecycle Configuration.
--
-- 'lastModifiedTime', 'studioLifecycleConfigDetails_lastModifiedTime' - This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
--
-- 'studioLifecycleConfigAppType', 'studioLifecycleConfigDetails_studioLifecycleConfigAppType' - The App type to which the Lifecycle Configuration is attached.
--
-- 'studioLifecycleConfigArn', 'studioLifecycleConfigDetails_studioLifecycleConfigArn' - The Amazon Resource Name (ARN) of the Lifecycle Configuration.
--
-- 'studioLifecycleConfigName', 'studioLifecycleConfigDetails_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration.
newStudioLifecycleConfigDetails ::
  StudioLifecycleConfigDetails
newStudioLifecycleConfigDetails =
  StudioLifecycleConfigDetails'
    { creationTime =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      studioLifecycleConfigAppType =
        Prelude.Nothing,
      studioLifecycleConfigArn = Prelude.Nothing,
      studioLifecycleConfigName = Prelude.Nothing
    }

-- | The creation time of the Studio Lifecycle Configuration.
studioLifecycleConfigDetails_creationTime :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.UTCTime)
studioLifecycleConfigDetails_creationTime = Lens.lens (\StudioLifecycleConfigDetails' {creationTime} -> creationTime) (\s@StudioLifecycleConfigDetails' {} a -> s {creationTime = a} :: StudioLifecycleConfigDetails) Prelude.. Lens.mapping Data._Time

-- | This value is equivalent to CreationTime because Studio Lifecycle
-- Configurations are immutable.
studioLifecycleConfigDetails_lastModifiedTime :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.UTCTime)
studioLifecycleConfigDetails_lastModifiedTime = Lens.lens (\StudioLifecycleConfigDetails' {lastModifiedTime} -> lastModifiedTime) (\s@StudioLifecycleConfigDetails' {} a -> s {lastModifiedTime = a} :: StudioLifecycleConfigDetails) Prelude.. Lens.mapping Data._Time

-- | The App type to which the Lifecycle Configuration is attached.
studioLifecycleConfigDetails_studioLifecycleConfigAppType :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe StudioLifecycleConfigAppType)
studioLifecycleConfigDetails_studioLifecycleConfigAppType = Lens.lens (\StudioLifecycleConfigDetails' {studioLifecycleConfigAppType} -> studioLifecycleConfigAppType) (\s@StudioLifecycleConfigDetails' {} a -> s {studioLifecycleConfigAppType = a} :: StudioLifecycleConfigDetails)

-- | The Amazon Resource Name (ARN) of the Lifecycle Configuration.
studioLifecycleConfigDetails_studioLifecycleConfigArn :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.Text)
studioLifecycleConfigDetails_studioLifecycleConfigArn = Lens.lens (\StudioLifecycleConfigDetails' {studioLifecycleConfigArn} -> studioLifecycleConfigArn) (\s@StudioLifecycleConfigDetails' {} a -> s {studioLifecycleConfigArn = a} :: StudioLifecycleConfigDetails)

-- | The name of the Studio Lifecycle Configuration.
studioLifecycleConfigDetails_studioLifecycleConfigName :: Lens.Lens' StudioLifecycleConfigDetails (Prelude.Maybe Prelude.Text)
studioLifecycleConfigDetails_studioLifecycleConfigName = Lens.lens (\StudioLifecycleConfigDetails' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@StudioLifecycleConfigDetails' {} a -> s {studioLifecycleConfigName = a} :: StudioLifecycleConfigDetails)

instance Data.FromJSON StudioLifecycleConfigDetails where
  parseJSON =
    Data.withObject
      "StudioLifecycleConfigDetails"
      ( \x ->
          StudioLifecycleConfigDetails'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "StudioLifecycleConfigAppType")
            Prelude.<*> (x Data..:? "StudioLifecycleConfigArn")
            Prelude.<*> (x Data..:? "StudioLifecycleConfigName")
      )

instance
  Prelude.Hashable
    StudioLifecycleConfigDetails
  where
  hashWithSalt _salt StudioLifecycleConfigDetails' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` studioLifecycleConfigAppType
      `Prelude.hashWithSalt` studioLifecycleConfigArn
      `Prelude.hashWithSalt` studioLifecycleConfigName

instance Prelude.NFData StudioLifecycleConfigDetails where
  rnf StudioLifecycleConfigDetails' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf studioLifecycleConfigAppType
      `Prelude.seq` Prelude.rnf studioLifecycleConfigArn
      `Prelude.seq` Prelude.rnf studioLifecycleConfigName
