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
-- Module      : Amazonka.IoTRoboRunner.Types.WorkerFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.WorkerFleet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A collection of workers organized within a facility.
--
-- /See:/ 'newWorkerFleet' smart constructor.
data WorkerFleet = WorkerFleet'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    site :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'workerFleet_additionalFixedProperties' - Undocumented member.
--
-- 'arn', 'workerFleet_arn' - Undocumented member.
--
-- 'id', 'workerFleet_id' - Undocumented member.
--
-- 'name', 'workerFleet_name' - Undocumented member.
--
-- 'site', 'workerFleet_site' - Undocumented member.
--
-- 'createdAt', 'workerFleet_createdAt' - Undocumented member.
--
-- 'updatedAt', 'workerFleet_updatedAt' - Undocumented member.
newWorkerFleet ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  WorkerFleet
newWorkerFleet
  pArn_
  pId_
  pName_
  pSite_
  pCreatedAt_
  pUpdatedAt_ =
    WorkerFleet'
      { additionalFixedProperties =
          Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        name = pName_,
        site = pSite_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | Undocumented member.
workerFleet_additionalFixedProperties :: Lens.Lens' WorkerFleet (Prelude.Maybe Prelude.Text)
workerFleet_additionalFixedProperties = Lens.lens (\WorkerFleet' {additionalFixedProperties} -> additionalFixedProperties) (\s@WorkerFleet' {} a -> s {additionalFixedProperties = a} :: WorkerFleet)

-- | Undocumented member.
workerFleet_arn :: Lens.Lens' WorkerFleet Prelude.Text
workerFleet_arn = Lens.lens (\WorkerFleet' {arn} -> arn) (\s@WorkerFleet' {} a -> s {arn = a} :: WorkerFleet)

-- | Undocumented member.
workerFleet_id :: Lens.Lens' WorkerFleet Prelude.Text
workerFleet_id = Lens.lens (\WorkerFleet' {id} -> id) (\s@WorkerFleet' {} a -> s {id = a} :: WorkerFleet)

-- | Undocumented member.
workerFleet_name :: Lens.Lens' WorkerFleet Prelude.Text
workerFleet_name = Lens.lens (\WorkerFleet' {name} -> name) (\s@WorkerFleet' {} a -> s {name = a} :: WorkerFleet)

-- | Undocumented member.
workerFleet_site :: Lens.Lens' WorkerFleet Prelude.Text
workerFleet_site = Lens.lens (\WorkerFleet' {site} -> site) (\s@WorkerFleet' {} a -> s {site = a} :: WorkerFleet)

-- | Undocumented member.
workerFleet_createdAt :: Lens.Lens' WorkerFleet Prelude.UTCTime
workerFleet_createdAt = Lens.lens (\WorkerFleet' {createdAt} -> createdAt) (\s@WorkerFleet' {} a -> s {createdAt = a} :: WorkerFleet) Prelude.. Data._Time

-- | Undocumented member.
workerFleet_updatedAt :: Lens.Lens' WorkerFleet Prelude.UTCTime
workerFleet_updatedAt = Lens.lens (\WorkerFleet' {updatedAt} -> updatedAt) (\s@WorkerFleet' {} a -> s {updatedAt = a} :: WorkerFleet) Prelude.. Data._Time

instance Data.FromJSON WorkerFleet where
  parseJSON =
    Data.withObject
      "WorkerFleet"
      ( \x ->
          WorkerFleet'
            Prelude.<$> (x Data..:? "additionalFixedProperties")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "site")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
      )

instance Prelude.Hashable WorkerFleet where
  hashWithSalt _salt WorkerFleet' {..} =
    _salt
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` site
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData WorkerFleet where
  rnf WorkerFleet' {..} =
    Prelude.rnf additionalFixedProperties `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf site `Prelude.seq`
              Prelude.rnf createdAt `Prelude.seq`
                Prelude.rnf updatedAt
