{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
  ( PullRequestStatusChangedEventMetadata (..)
  -- * Smart constructor
  , mkPullRequestStatusChangedEventMetadata
  -- * Lenses
  , prscemPullRequestStatus
  ) where

import qualified Network.AWS.CodeCommit.Types.PullRequestStatusEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a change to the status of a pull request.
--
-- /See:/ 'mkPullRequestStatusChangedEventMetadata' smart constructor.
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata'
  { pullRequestStatus :: Core.Maybe Types.PullRequestStatusEnum
    -- ^ The changed status of the pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PullRequestStatusChangedEventMetadata' value with any optional fields omitted.
mkPullRequestStatusChangedEventMetadata
    :: PullRequestStatusChangedEventMetadata
mkPullRequestStatusChangedEventMetadata
  = PullRequestStatusChangedEventMetadata'{pullRequestStatus =
                                             Core.Nothing}

-- | The changed status of the pull request.
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prscemPullRequestStatus :: Lens.Lens' PullRequestStatusChangedEventMetadata (Core.Maybe Types.PullRequestStatusEnum)
prscemPullRequestStatus = Lens.field @"pullRequestStatus"
{-# INLINEABLE prscemPullRequestStatus #-}
{-# DEPRECATED pullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead"  #-}

instance Core.FromJSON PullRequestStatusChangedEventMetadata where
        parseJSON
          = Core.withObject "PullRequestStatusChangedEventMetadata" Core.$
              \ x ->
                PullRequestStatusChangedEventMetadata' Core.<$>
                  (x Core..:? "pullRequestStatus")
