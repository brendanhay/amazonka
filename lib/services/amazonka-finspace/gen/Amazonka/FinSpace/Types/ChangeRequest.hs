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
-- Module      : Amazonka.FinSpace.Types.ChangeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.ChangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.ChangeType
import qualified Amazonka.Prelude as Prelude

-- | A list of change request objects.
--
-- /See:/ 'newChangeRequest' smart constructor.
data ChangeRequest = ChangeRequest'
  { -- | Defines the S3 path of the source file that is required to add or update
    -- files in a database.
    s3Path :: Prelude.Maybe Prelude.Text,
    -- | Defines the type of change request. A @changeType@ can have the
    -- following values:
    --
    -- -   PUT – Adds or updates files in a database.
    --
    -- -   DELETE – Deletes files in a database.
    changeType :: ChangeType,
    -- | Defines the path within the database directory.
    dbPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Path', 'changeRequest_s3Path' - Defines the S3 path of the source file that is required to add or update
-- files in a database.
--
-- 'changeType', 'changeRequest_changeType' - Defines the type of change request. A @changeType@ can have the
-- following values:
--
-- -   PUT – Adds or updates files in a database.
--
-- -   DELETE – Deletes files in a database.
--
-- 'dbPath', 'changeRequest_dbPath' - Defines the path within the database directory.
newChangeRequest ::
  -- | 'changeType'
  ChangeType ->
  -- | 'dbPath'
  Prelude.Text ->
  ChangeRequest
newChangeRequest pChangeType_ pDbPath_ =
  ChangeRequest'
    { s3Path = Prelude.Nothing,
      changeType = pChangeType_,
      dbPath = pDbPath_
    }

-- | Defines the S3 path of the source file that is required to add or update
-- files in a database.
changeRequest_s3Path :: Lens.Lens' ChangeRequest (Prelude.Maybe Prelude.Text)
changeRequest_s3Path = Lens.lens (\ChangeRequest' {s3Path} -> s3Path) (\s@ChangeRequest' {} a -> s {s3Path = a} :: ChangeRequest)

-- | Defines the type of change request. A @changeType@ can have the
-- following values:
--
-- -   PUT – Adds or updates files in a database.
--
-- -   DELETE – Deletes files in a database.
changeRequest_changeType :: Lens.Lens' ChangeRequest ChangeType
changeRequest_changeType = Lens.lens (\ChangeRequest' {changeType} -> changeType) (\s@ChangeRequest' {} a -> s {changeType = a} :: ChangeRequest)

-- | Defines the path within the database directory.
changeRequest_dbPath :: Lens.Lens' ChangeRequest Prelude.Text
changeRequest_dbPath = Lens.lens (\ChangeRequest' {dbPath} -> dbPath) (\s@ChangeRequest' {} a -> s {dbPath = a} :: ChangeRequest)

instance Data.FromJSON ChangeRequest where
  parseJSON =
    Data.withObject
      "ChangeRequest"
      ( \x ->
          ChangeRequest'
            Prelude.<$> (x Data..:? "s3Path")
            Prelude.<*> (x Data..: "changeType")
            Prelude.<*> (x Data..: "dbPath")
      )

instance Prelude.Hashable ChangeRequest where
  hashWithSalt _salt ChangeRequest' {..} =
    _salt
      `Prelude.hashWithSalt` s3Path
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` dbPath

instance Prelude.NFData ChangeRequest where
  rnf ChangeRequest' {..} =
    Prelude.rnf s3Path
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf dbPath

instance Data.ToJSON ChangeRequest where
  toJSON ChangeRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3Path" Data..=) Prelude.<$> s3Path,
            Prelude.Just ("changeType" Data..= changeType),
            Prelude.Just ("dbPath" Data..= dbPath)
          ]
      )
