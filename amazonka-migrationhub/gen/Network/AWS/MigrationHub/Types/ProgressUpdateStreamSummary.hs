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
-- Module      : Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary of the AWS resource used for access control that is implicitly
-- linked to your AWS account.
--
-- /See:/ 'newProgressUpdateStreamSummary' smart constructor.
data ProgressUpdateStreamSummary = ProgressUpdateStreamSummary'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in
    -- this field./
    progressUpdateStreamName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
progressUpdateStreamSummary_progressUpdateStreamName :: Lens.Lens' ProgressUpdateStreamSummary (Core.Maybe Core.Text)
progressUpdateStreamSummary_progressUpdateStreamName = Lens.lens (\ProgressUpdateStreamSummary' {progressUpdateStreamName} -> progressUpdateStreamName) (\s@ProgressUpdateStreamSummary' {} a -> s {progressUpdateStreamName = a} :: ProgressUpdateStreamSummary)

instance Core.FromJSON ProgressUpdateStreamSummary where
  parseJSON =
    Core.withObject
      "ProgressUpdateStreamSummary"
      ( \x ->
          ProgressUpdateStreamSummary'
            Core.<$> (x Core..:? "ProgressUpdateStreamName")
      )

instance Core.Hashable ProgressUpdateStreamSummary

instance Core.NFData ProgressUpdateStreamSummary
