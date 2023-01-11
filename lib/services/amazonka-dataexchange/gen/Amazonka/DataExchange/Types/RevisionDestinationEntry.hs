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
-- Module      : Amazonka.DataExchange.Types.RevisionDestinationEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.RevisionDestinationEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The destination where the assets in the revision will be exported.
--
-- /See:/ 'newRevisionDestinationEntry' smart constructor.
data RevisionDestinationEntry = RevisionDestinationEntry'
  { -- | A string representing the pattern for generated names of the individual
    -- assets in the revision. For more information about key patterns, see
    -- <https://docs.aws.amazon.com/data-exchange/latest/userguide/jobs.html#revision-export-keypatterns Key patterns when exporting revisions>.
    keyPattern :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that is the destination for the assets in the
    -- revision.
    bucket :: Prelude.Text,
    -- | The unique identifier for the revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevisionDestinationEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPattern', 'revisionDestinationEntry_keyPattern' - A string representing the pattern for generated names of the individual
-- assets in the revision. For more information about key patterns, see
-- <https://docs.aws.amazon.com/data-exchange/latest/userguide/jobs.html#revision-export-keypatterns Key patterns when exporting revisions>.
--
-- 'bucket', 'revisionDestinationEntry_bucket' - The Amazon S3 bucket that is the destination for the assets in the
-- revision.
--
-- 'revisionId', 'revisionDestinationEntry_revisionId' - The unique identifier for the revision.
newRevisionDestinationEntry ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  RevisionDestinationEntry
newRevisionDestinationEntry pBucket_ pRevisionId_ =
  RevisionDestinationEntry'
    { keyPattern =
        Prelude.Nothing,
      bucket = pBucket_,
      revisionId = pRevisionId_
    }

-- | A string representing the pattern for generated names of the individual
-- assets in the revision. For more information about key patterns, see
-- <https://docs.aws.amazon.com/data-exchange/latest/userguide/jobs.html#revision-export-keypatterns Key patterns when exporting revisions>.
revisionDestinationEntry_keyPattern :: Lens.Lens' RevisionDestinationEntry (Prelude.Maybe Prelude.Text)
revisionDestinationEntry_keyPattern = Lens.lens (\RevisionDestinationEntry' {keyPattern} -> keyPattern) (\s@RevisionDestinationEntry' {} a -> s {keyPattern = a} :: RevisionDestinationEntry)

-- | The Amazon S3 bucket that is the destination for the assets in the
-- revision.
revisionDestinationEntry_bucket :: Lens.Lens' RevisionDestinationEntry Prelude.Text
revisionDestinationEntry_bucket = Lens.lens (\RevisionDestinationEntry' {bucket} -> bucket) (\s@RevisionDestinationEntry' {} a -> s {bucket = a} :: RevisionDestinationEntry)

-- | The unique identifier for the revision.
revisionDestinationEntry_revisionId :: Lens.Lens' RevisionDestinationEntry Prelude.Text
revisionDestinationEntry_revisionId = Lens.lens (\RevisionDestinationEntry' {revisionId} -> revisionId) (\s@RevisionDestinationEntry' {} a -> s {revisionId = a} :: RevisionDestinationEntry)

instance Data.FromJSON RevisionDestinationEntry where
  parseJSON =
    Data.withObject
      "RevisionDestinationEntry"
      ( \x ->
          RevisionDestinationEntry'
            Prelude.<$> (x Data..:? "KeyPattern")
            Prelude.<*> (x Data..: "Bucket")
            Prelude.<*> (x Data..: "RevisionId")
      )

instance Prelude.Hashable RevisionDestinationEntry where
  hashWithSalt _salt RevisionDestinationEntry' {..} =
    _salt `Prelude.hashWithSalt` keyPattern
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData RevisionDestinationEntry where
  rnf RevisionDestinationEntry' {..} =
    Prelude.rnf keyPattern
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf revisionId

instance Data.ToJSON RevisionDestinationEntry where
  toJSON RevisionDestinationEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyPattern" Data..=) Prelude.<$> keyPattern,
            Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("RevisionId" Data..= revisionId)
          ]
      )
