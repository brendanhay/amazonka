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
-- Module      : Amazonka.DataExchange.Types.AutoExportRevisionDestinationEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AutoExportRevisionDestinationEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A revision destination is the Amazon S3 bucket folder destination to
-- where the export will be sent.
--
-- /See:/ 'newAutoExportRevisionDestinationEntry' smart constructor.
data AutoExportRevisionDestinationEntry = AutoExportRevisionDestinationEntry'
  { -- | A string representing the pattern for generated names of the individual
    -- assets in the revision. For more information about key patterns, see
    -- <https://docs.aws.amazon.com/data-exchange/latest/userguide/jobs.html#revision-export-keypatterns Key patterns when exporting revisions>.
    keyPattern :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket that is the destination for the event action.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoExportRevisionDestinationEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPattern', 'autoExportRevisionDestinationEntry_keyPattern' - A string representing the pattern for generated names of the individual
-- assets in the revision. For more information about key patterns, see
-- <https://docs.aws.amazon.com/data-exchange/latest/userguide/jobs.html#revision-export-keypatterns Key patterns when exporting revisions>.
--
-- 'bucket', 'autoExportRevisionDestinationEntry_bucket' - The S3 bucket that is the destination for the event action.
newAutoExportRevisionDestinationEntry ::
  -- | 'bucket'
  Prelude.Text ->
  AutoExportRevisionDestinationEntry
newAutoExportRevisionDestinationEntry pBucket_ =
  AutoExportRevisionDestinationEntry'
    { keyPattern =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | A string representing the pattern for generated names of the individual
-- assets in the revision. For more information about key patterns, see
-- <https://docs.aws.amazon.com/data-exchange/latest/userguide/jobs.html#revision-export-keypatterns Key patterns when exporting revisions>.
autoExportRevisionDestinationEntry_keyPattern :: Lens.Lens' AutoExportRevisionDestinationEntry (Prelude.Maybe Prelude.Text)
autoExportRevisionDestinationEntry_keyPattern = Lens.lens (\AutoExportRevisionDestinationEntry' {keyPattern} -> keyPattern) (\s@AutoExportRevisionDestinationEntry' {} a -> s {keyPattern = a} :: AutoExportRevisionDestinationEntry)

-- | The S3 bucket that is the destination for the event action.
autoExportRevisionDestinationEntry_bucket :: Lens.Lens' AutoExportRevisionDestinationEntry Prelude.Text
autoExportRevisionDestinationEntry_bucket = Lens.lens (\AutoExportRevisionDestinationEntry' {bucket} -> bucket) (\s@AutoExportRevisionDestinationEntry' {} a -> s {bucket = a} :: AutoExportRevisionDestinationEntry)

instance
  Data.FromJSON
    AutoExportRevisionDestinationEntry
  where
  parseJSON =
    Data.withObject
      "AutoExportRevisionDestinationEntry"
      ( \x ->
          AutoExportRevisionDestinationEntry'
            Prelude.<$> (x Data..:? "KeyPattern")
            Prelude.<*> (x Data..: "Bucket")
      )

instance
  Prelude.Hashable
    AutoExportRevisionDestinationEntry
  where
  hashWithSalt
    _salt
    AutoExportRevisionDestinationEntry' {..} =
      _salt `Prelude.hashWithSalt` keyPattern
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    AutoExportRevisionDestinationEntry
  where
  rnf AutoExportRevisionDestinationEntry' {..} =
    Prelude.rnf keyPattern
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToJSON
    AutoExportRevisionDestinationEntry
  where
  toJSON AutoExportRevisionDestinationEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyPattern" Data..=) Prelude.<$> keyPattern,
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
