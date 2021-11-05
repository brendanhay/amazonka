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
-- Module      : Amazonka.LexModels.Types.MigrationAlert
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.MigrationAlert where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexModels.Types.MigrationAlertType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about alerts and warnings that Amazon Lex sends
-- during a migration. The alerts include information about how to resolve
-- the issue.
--
-- /See:/ 'newMigrationAlert' smart constructor.
data MigrationAlert = MigrationAlert'
  { -- | A link to the Amazon Lex documentation that describes how to resolve the
    -- alert.
    referenceURLs :: Prelude.Maybe [Prelude.Text],
    -- | Additional details about the alert.
    details :: Prelude.Maybe [Prelude.Text],
    -- | The type of alert. There are two kinds of alerts:
    --
    -- -   @ERROR@ - There was an issue with the migration that can\'t be
    --     resolved. The migration stops.
    --
    -- -   @WARN@ - There was an issue with the migration that requires manual
    --     changes to the new Amazon Lex V2 bot. The migration continues.
    type' :: Prelude.Maybe MigrationAlertType,
    -- | A message that describes why the alert was issued.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MigrationAlert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceURLs', 'migrationAlert_referenceURLs' - A link to the Amazon Lex documentation that describes how to resolve the
-- alert.
--
-- 'details', 'migrationAlert_details' - Additional details about the alert.
--
-- 'type'', 'migrationAlert_type' - The type of alert. There are two kinds of alerts:
--
-- -   @ERROR@ - There was an issue with the migration that can\'t be
--     resolved. The migration stops.
--
-- -   @WARN@ - There was an issue with the migration that requires manual
--     changes to the new Amazon Lex V2 bot. The migration continues.
--
-- 'message', 'migrationAlert_message' - A message that describes why the alert was issued.
newMigrationAlert ::
  MigrationAlert
newMigrationAlert =
  MigrationAlert'
    { referenceURLs = Prelude.Nothing,
      details = Prelude.Nothing,
      type' = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | A link to the Amazon Lex documentation that describes how to resolve the
-- alert.
migrationAlert_referenceURLs :: Lens.Lens' MigrationAlert (Prelude.Maybe [Prelude.Text])
migrationAlert_referenceURLs = Lens.lens (\MigrationAlert' {referenceURLs} -> referenceURLs) (\s@MigrationAlert' {} a -> s {referenceURLs = a} :: MigrationAlert) Prelude.. Lens.mapping Lens.coerced

-- | Additional details about the alert.
migrationAlert_details :: Lens.Lens' MigrationAlert (Prelude.Maybe [Prelude.Text])
migrationAlert_details = Lens.lens (\MigrationAlert' {details} -> details) (\s@MigrationAlert' {} a -> s {details = a} :: MigrationAlert) Prelude.. Lens.mapping Lens.coerced

-- | The type of alert. There are two kinds of alerts:
--
-- -   @ERROR@ - There was an issue with the migration that can\'t be
--     resolved. The migration stops.
--
-- -   @WARN@ - There was an issue with the migration that requires manual
--     changes to the new Amazon Lex V2 bot. The migration continues.
migrationAlert_type :: Lens.Lens' MigrationAlert (Prelude.Maybe MigrationAlertType)
migrationAlert_type = Lens.lens (\MigrationAlert' {type'} -> type') (\s@MigrationAlert' {} a -> s {type' = a} :: MigrationAlert)

-- | A message that describes why the alert was issued.
migrationAlert_message :: Lens.Lens' MigrationAlert (Prelude.Maybe Prelude.Text)
migrationAlert_message = Lens.lens (\MigrationAlert' {message} -> message) (\s@MigrationAlert' {} a -> s {message = a} :: MigrationAlert)

instance Core.FromJSON MigrationAlert where
  parseJSON =
    Core.withObject
      "MigrationAlert"
      ( \x ->
          MigrationAlert'
            Prelude.<$> (x Core..:? "referenceURLs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "details" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable MigrationAlert

instance Prelude.NFData MigrationAlert
