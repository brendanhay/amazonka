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
-- Module      : Amazonka.AMP.Types.AlertManagerDefinitionDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.AlertManagerDefinitionDescription where

import Amazonka.AMP.Types.AlertManagerDefinitionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of an alert manager definition.
--
-- /See:/ 'newAlertManagerDefinitionDescription' smart constructor.
data AlertManagerDefinitionDescription = AlertManagerDefinitionDescription'
  { -- | The time when the alert manager definition was created.
    createdAt :: Data.POSIX,
    -- | The alert manager definition.
    data' :: Data.Base64,
    -- | The time when the alert manager definition was modified.
    modifiedAt :: Data.POSIX,
    -- | The status of alert manager definition.
    status :: AlertManagerDefinitionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlertManagerDefinitionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'alertManagerDefinitionDescription_createdAt' - The time when the alert manager definition was created.
--
-- 'data'', 'alertManagerDefinitionDescription_data' - The alert manager definition.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'modifiedAt', 'alertManagerDefinitionDescription_modifiedAt' - The time when the alert manager definition was modified.
--
-- 'status', 'alertManagerDefinitionDescription_status' - The status of alert manager definition.
newAlertManagerDefinitionDescription ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'data''
  Prelude.ByteString ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  -- | 'status'
  AlertManagerDefinitionStatus ->
  AlertManagerDefinitionDescription
newAlertManagerDefinitionDescription
  pCreatedAt_
  pData_
  pModifiedAt_
  pStatus_ =
    AlertManagerDefinitionDescription'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        data' = Data._Base64 Lens.# pData_,
        modifiedAt =
          Data._Time Lens.# pModifiedAt_,
        status = pStatus_
      }

-- | The time when the alert manager definition was created.
alertManagerDefinitionDescription_createdAt :: Lens.Lens' AlertManagerDefinitionDescription Prelude.UTCTime
alertManagerDefinitionDescription_createdAt = Lens.lens (\AlertManagerDefinitionDescription' {createdAt} -> createdAt) (\s@AlertManagerDefinitionDescription' {} a -> s {createdAt = a} :: AlertManagerDefinitionDescription) Prelude.. Data._Time

-- | The alert manager definition.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
alertManagerDefinitionDescription_data :: Lens.Lens' AlertManagerDefinitionDescription Prelude.ByteString
alertManagerDefinitionDescription_data = Lens.lens (\AlertManagerDefinitionDescription' {data'} -> data') (\s@AlertManagerDefinitionDescription' {} a -> s {data' = a} :: AlertManagerDefinitionDescription) Prelude.. Data._Base64

-- | The time when the alert manager definition was modified.
alertManagerDefinitionDescription_modifiedAt :: Lens.Lens' AlertManagerDefinitionDescription Prelude.UTCTime
alertManagerDefinitionDescription_modifiedAt = Lens.lens (\AlertManagerDefinitionDescription' {modifiedAt} -> modifiedAt) (\s@AlertManagerDefinitionDescription' {} a -> s {modifiedAt = a} :: AlertManagerDefinitionDescription) Prelude.. Data._Time

-- | The status of alert manager definition.
alertManagerDefinitionDescription_status :: Lens.Lens' AlertManagerDefinitionDescription AlertManagerDefinitionStatus
alertManagerDefinitionDescription_status = Lens.lens (\AlertManagerDefinitionDescription' {status} -> status) (\s@AlertManagerDefinitionDescription' {} a -> s {status = a} :: AlertManagerDefinitionDescription)

instance
  Data.FromJSON
    AlertManagerDefinitionDescription
  where
  parseJSON =
    Data.withObject
      "AlertManagerDefinitionDescription"
      ( \x ->
          AlertManagerDefinitionDescription'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "data")
            Prelude.<*> (x Data..: "modifiedAt")
            Prelude.<*> (x Data..: "status")
      )

instance
  Prelude.Hashable
    AlertManagerDefinitionDescription
  where
  hashWithSalt
    _salt
    AlertManagerDefinitionDescription' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` data'
        `Prelude.hashWithSalt` modifiedAt
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AlertManagerDefinitionDescription
  where
  rnf AlertManagerDefinitionDescription' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf status
