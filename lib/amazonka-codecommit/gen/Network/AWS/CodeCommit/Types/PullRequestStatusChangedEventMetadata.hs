{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
  ( PullRequestStatusChangedEventMetadata (..),

    -- * Smart constructor
    mkPullRequestStatusChangedEventMetadata,

    -- * Lenses
    prscemPullRequestStatus,
  )
where

import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a change to the status of a pull request.
--
-- /See:/ 'mkPullRequestStatusChangedEventMetadata' smart constructor.
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata'
  { -- | The changed status of the pull request.
    pullRequestStatus :: Lude.Maybe PullRequestStatusEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequestStatusChangedEventMetadata' with the minimum fields required to make a request.
--
-- * 'pullRequestStatus' - The changed status of the pull request.
mkPullRequestStatusChangedEventMetadata ::
  PullRequestStatusChangedEventMetadata
mkPullRequestStatusChangedEventMetadata =
  PullRequestStatusChangedEventMetadata'
    { pullRequestStatus =
        Lude.Nothing
    }

-- | The changed status of the pull request.
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prscemPullRequestStatus :: Lens.Lens' PullRequestStatusChangedEventMetadata (Lude.Maybe PullRequestStatusEnum)
prscemPullRequestStatus = Lens.lens (pullRequestStatus :: PullRequestStatusChangedEventMetadata -> Lude.Maybe PullRequestStatusEnum) (\s a -> s {pullRequestStatus = a} :: PullRequestStatusChangedEventMetadata)
{-# DEPRECATED prscemPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

instance Lude.FromJSON PullRequestStatusChangedEventMetadata where
  parseJSON =
    Lude.withObject
      "PullRequestStatusChangedEventMetadata"
      ( \x ->
          PullRequestStatusChangedEventMetadata'
            Lude.<$> (x Lude..:? "pullRequestStatus")
      )
