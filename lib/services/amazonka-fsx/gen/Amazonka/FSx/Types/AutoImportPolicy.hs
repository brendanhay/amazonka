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
-- Module      : Amazonka.FSx.Types.AutoImportPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AutoImportPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.EventType
import qualified Amazonka.Prelude as Prelude

-- | Describes the data repository association\'s automatic import policy.
-- The AutoImportPolicy defines how Amazon FSx keeps your file metadata and
-- directory listings up to date by importing changes to your Amazon FSx
-- for Lustre file system as you modify objects in a linked S3 bucket.
--
-- The @AutoImportPolicy@ is only supported on Amazon FSx for Lustre file
-- systems with a data repository association.
--
-- /See:/ 'newAutoImportPolicy' smart constructor.
data AutoImportPolicy = AutoImportPolicy'
  { -- | The @AutoImportPolicy@ can have the following event values:
    --
    -- -   @NEW@ - Amazon FSx automatically imports metadata of files added to
    --     the linked S3 bucket that do not currently exist in the FSx file
    --     system.
    --
    -- -   @CHANGED@ - Amazon FSx automatically updates file metadata and
    --     invalidates existing file content on the file system as files change
    --     in the data repository.
    --
    -- -   @DELETED@ - Amazon FSx automatically deletes files on the file
    --     system as corresponding files are deleted in the data repository.
    --
    -- You can define any combination of event types for your
    -- @AutoImportPolicy@.
    events :: Prelude.Maybe [EventType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoImportPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'autoImportPolicy_events' - The @AutoImportPolicy@ can have the following event values:
--
-- -   @NEW@ - Amazon FSx automatically imports metadata of files added to
--     the linked S3 bucket that do not currently exist in the FSx file
--     system.
--
-- -   @CHANGED@ - Amazon FSx automatically updates file metadata and
--     invalidates existing file content on the file system as files change
--     in the data repository.
--
-- -   @DELETED@ - Amazon FSx automatically deletes files on the file
--     system as corresponding files are deleted in the data repository.
--
-- You can define any combination of event types for your
-- @AutoImportPolicy@.
newAutoImportPolicy ::
  AutoImportPolicy
newAutoImportPolicy =
  AutoImportPolicy' {events = Prelude.Nothing}

-- | The @AutoImportPolicy@ can have the following event values:
--
-- -   @NEW@ - Amazon FSx automatically imports metadata of files added to
--     the linked S3 bucket that do not currently exist in the FSx file
--     system.
--
-- -   @CHANGED@ - Amazon FSx automatically updates file metadata and
--     invalidates existing file content on the file system as files change
--     in the data repository.
--
-- -   @DELETED@ - Amazon FSx automatically deletes files on the file
--     system as corresponding files are deleted in the data repository.
--
-- You can define any combination of event types for your
-- @AutoImportPolicy@.
autoImportPolicy_events :: Lens.Lens' AutoImportPolicy (Prelude.Maybe [EventType])
autoImportPolicy_events = Lens.lens (\AutoImportPolicy' {events} -> events) (\s@AutoImportPolicy' {} a -> s {events = a} :: AutoImportPolicy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutoImportPolicy where
  parseJSON =
    Data.withObject
      "AutoImportPolicy"
      ( \x ->
          AutoImportPolicy'
            Prelude.<$> (x Data..:? "Events" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AutoImportPolicy where
  hashWithSalt _salt AutoImportPolicy' {..} =
    _salt `Prelude.hashWithSalt` events

instance Prelude.NFData AutoImportPolicy where
  rnf AutoImportPolicy' {..} = Prelude.rnf events

instance Data.ToJSON AutoImportPolicy where
  toJSON AutoImportPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Events" Data..=) Prelude.<$> events]
      )
