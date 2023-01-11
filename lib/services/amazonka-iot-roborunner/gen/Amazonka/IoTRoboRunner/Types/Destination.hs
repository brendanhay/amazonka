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
-- Module      : Amazonka.IoTRoboRunner.Types.Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types.DestinationState
import qualified Amazonka.Prelude as Prelude

-- | Area within a facility where work can be performed.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    site :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX,
    state :: DestinationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'destination_additionalFixedProperties' - Undocumented member.
--
-- 'arn', 'destination_arn' - Undocumented member.
--
-- 'id', 'destination_id' - Undocumented member.
--
-- 'name', 'destination_name' - Undocumented member.
--
-- 'site', 'destination_site' - Undocumented member.
--
-- 'createdAt', 'destination_createdAt' - Undocumented member.
--
-- 'updatedAt', 'destination_updatedAt' - Undocumented member.
--
-- 'state', 'destination_state' - Undocumented member.
newDestination ::
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
  -- | 'state'
  DestinationState ->
  Destination
newDestination
  pArn_
  pId_
  pName_
  pSite_
  pCreatedAt_
  pUpdatedAt_
  pState_ =
    Destination'
      { additionalFixedProperties =
          Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        name = pName_,
        site = pSite_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        state = pState_
      }

-- | Undocumented member.
destination_additionalFixedProperties :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_additionalFixedProperties = Lens.lens (\Destination' {additionalFixedProperties} -> additionalFixedProperties) (\s@Destination' {} a -> s {additionalFixedProperties = a} :: Destination)

-- | Undocumented member.
destination_arn :: Lens.Lens' Destination Prelude.Text
destination_arn = Lens.lens (\Destination' {arn} -> arn) (\s@Destination' {} a -> s {arn = a} :: Destination)

-- | Undocumented member.
destination_id :: Lens.Lens' Destination Prelude.Text
destination_id = Lens.lens (\Destination' {id} -> id) (\s@Destination' {} a -> s {id = a} :: Destination)

-- | Undocumented member.
destination_name :: Lens.Lens' Destination Prelude.Text
destination_name = Lens.lens (\Destination' {name} -> name) (\s@Destination' {} a -> s {name = a} :: Destination)

-- | Undocumented member.
destination_site :: Lens.Lens' Destination Prelude.Text
destination_site = Lens.lens (\Destination' {site} -> site) (\s@Destination' {} a -> s {site = a} :: Destination)

-- | Undocumented member.
destination_createdAt :: Lens.Lens' Destination Prelude.UTCTime
destination_createdAt = Lens.lens (\Destination' {createdAt} -> createdAt) (\s@Destination' {} a -> s {createdAt = a} :: Destination) Prelude.. Data._Time

-- | Undocumented member.
destination_updatedAt :: Lens.Lens' Destination Prelude.UTCTime
destination_updatedAt = Lens.lens (\Destination' {updatedAt} -> updatedAt) (\s@Destination' {} a -> s {updatedAt = a} :: Destination) Prelude.. Data._Time

-- | Undocumented member.
destination_state :: Lens.Lens' Destination DestinationState
destination_state = Lens.lens (\Destination' {state} -> state) (\s@Destination' {} a -> s {state = a} :: Destination)

instance Data.FromJSON Destination where
  parseJSON =
    Data.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Data..:? "additionalFixedProperties")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "site")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` site
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` state

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf site
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf state
