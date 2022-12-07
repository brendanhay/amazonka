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
-- Module      : Amazonka.MigrationHub.Types.ProgressUpdateStreamSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types.ProgressUpdateStreamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the AWS resource used for access control that is implicitly
-- linked to your AWS account.
--
-- /See:/ 'newProgressUpdateStreamSummary' smart constructor.
data ProgressUpdateStreamSummary = ProgressUpdateStreamSummary'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in
    -- this field./
    progressUpdateStreamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProgressUpdateStreamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressUpdateStreamName', 'progressUpdateStreamSummary_progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
newProgressUpdateStreamSummary ::
  ProgressUpdateStreamSummary
newProgressUpdateStreamSummary =
  ProgressUpdateStreamSummary'
    { progressUpdateStreamName =
        Prelude.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
progressUpdateStreamSummary_progressUpdateStreamName :: Lens.Lens' ProgressUpdateStreamSummary (Prelude.Maybe Prelude.Text)
progressUpdateStreamSummary_progressUpdateStreamName = Lens.lens (\ProgressUpdateStreamSummary' {progressUpdateStreamName} -> progressUpdateStreamName) (\s@ProgressUpdateStreamSummary' {} a -> s {progressUpdateStreamName = a} :: ProgressUpdateStreamSummary)

instance Data.FromJSON ProgressUpdateStreamSummary where
  parseJSON =
    Data.withObject
      "ProgressUpdateStreamSummary"
      ( \x ->
          ProgressUpdateStreamSummary'
            Prelude.<$> (x Data..:? "ProgressUpdateStreamName")
      )

instance Prelude.Hashable ProgressUpdateStreamSummary where
  hashWithSalt _salt ProgressUpdateStreamSummary' {..} =
    _salt
      `Prelude.hashWithSalt` progressUpdateStreamName

instance Prelude.NFData ProgressUpdateStreamSummary where
  rnf ProgressUpdateStreamSummary' {..} =
    Prelude.rnf progressUpdateStreamName
