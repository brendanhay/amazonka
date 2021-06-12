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
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata where

import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a change to the status of a pull request.
--
-- /See:/ 'newPullRequestStatusChangedEventMetadata' smart constructor.
data PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata'
  { -- | The changed status of the pull request.
    pullRequestStatus :: Core.Maybe PullRequestStatusEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PullRequestStatusChangedEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestStatus', 'pullRequestStatusChangedEventMetadata_pullRequestStatus' - The changed status of the pull request.
newPullRequestStatusChangedEventMetadata ::
  PullRequestStatusChangedEventMetadata
newPullRequestStatusChangedEventMetadata =
  PullRequestStatusChangedEventMetadata'
    { pullRequestStatus =
        Core.Nothing
    }

-- | The changed status of the pull request.
pullRequestStatusChangedEventMetadata_pullRequestStatus :: Lens.Lens' PullRequestStatusChangedEventMetadata (Core.Maybe PullRequestStatusEnum)
pullRequestStatusChangedEventMetadata_pullRequestStatus = Lens.lens (\PullRequestStatusChangedEventMetadata' {pullRequestStatus} -> pullRequestStatus) (\s@PullRequestStatusChangedEventMetadata' {} a -> s {pullRequestStatus = a} :: PullRequestStatusChangedEventMetadata)

instance
  Core.FromJSON
    PullRequestStatusChangedEventMetadata
  where
  parseJSON =
    Core.withObject
      "PullRequestStatusChangedEventMetadata"
      ( \x ->
          PullRequestStatusChangedEventMetadata'
            Core.<$> (x Core..:? "pullRequestStatus")
      )

instance
  Core.Hashable
    PullRequestStatusChangedEventMetadata

instance
  Core.NFData
    PullRequestStatusChangedEventMetadata
