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
-- Module      : Amazonka.FSx.Types.AutoExportPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AutoExportPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.EventType
import qualified Amazonka.Prelude as Prelude

-- | Describes a data repository association\'s automatic export policy. The
-- @AutoExportPolicy@ defines the types of updated objects on the file
-- system that will be automatically exported to the data repository. As
-- you create, modify, or delete files, Amazon FSx for Lustre automatically
-- exports the defined changes asynchronously once your application
-- finishes modifying the file.
--
-- This @AutoExportPolicy@ is supported only for Amazon FSx for Lustre file
-- systems with the @Persistent_2@ deployment type.
--
-- /See:/ 'newAutoExportPolicy' smart constructor.
data AutoExportPolicy = AutoExportPolicy'
  { -- | The @AutoExportPolicy@ can have the following event values:
    --
    -- -   @NEW@ - New files and directories are automatically exported to the
    --     data repository as they are added to the file system.
    --
    -- -   @CHANGED@ - Changes to files and directories on the file system are
    --     automatically exported to the data repository.
    --
    -- -   @DELETED@ - Files and directories are automatically deleted on the
    --     data repository when they are deleted on the file system.
    --
    -- You can define any combination of event types for your
    -- @AutoExportPolicy@.
    events :: Prelude.Maybe [EventType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoExportPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'autoExportPolicy_events' - The @AutoExportPolicy@ can have the following event values:
--
-- -   @NEW@ - New files and directories are automatically exported to the
--     data repository as they are added to the file system.
--
-- -   @CHANGED@ - Changes to files and directories on the file system are
--     automatically exported to the data repository.
--
-- -   @DELETED@ - Files and directories are automatically deleted on the
--     data repository when they are deleted on the file system.
--
-- You can define any combination of event types for your
-- @AutoExportPolicy@.
newAutoExportPolicy ::
  AutoExportPolicy
newAutoExportPolicy =
  AutoExportPolicy' {events = Prelude.Nothing}

-- | The @AutoExportPolicy@ can have the following event values:
--
-- -   @NEW@ - New files and directories are automatically exported to the
--     data repository as they are added to the file system.
--
-- -   @CHANGED@ - Changes to files and directories on the file system are
--     automatically exported to the data repository.
--
-- -   @DELETED@ - Files and directories are automatically deleted on the
--     data repository when they are deleted on the file system.
--
-- You can define any combination of event types for your
-- @AutoExportPolicy@.
autoExportPolicy_events :: Lens.Lens' AutoExportPolicy (Prelude.Maybe [EventType])
autoExportPolicy_events = Lens.lens (\AutoExportPolicy' {events} -> events) (\s@AutoExportPolicy' {} a -> s {events = a} :: AutoExportPolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutoExportPolicy where
  parseJSON =
    Data.withObject
      "AutoExportPolicy"
      ( \x ->
          AutoExportPolicy'
            Prelude.<$> (x Data..:? "Events" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AutoExportPolicy where
  hashWithSalt _salt AutoExportPolicy' {..} =
    _salt `Prelude.hashWithSalt` events

instance Prelude.NFData AutoExportPolicy where
  rnf AutoExportPolicy' {..} = Prelude.rnf events

instance Data.ToJSON AutoExportPolicy where
  toJSON AutoExportPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Events" Data..=) Prelude.<$> events]
      )
