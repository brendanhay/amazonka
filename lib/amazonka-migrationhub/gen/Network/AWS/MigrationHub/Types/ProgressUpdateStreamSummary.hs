{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
  ( ProgressUpdateStreamSummary (..),

    -- * Smart constructor
    mkProgressUpdateStreamSummary,

    -- * Lenses
    pussProgressUpdateStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary of the AWS resource used for access control that is implicitly linked to your AWS account.
--
-- /See:/ 'mkProgressUpdateStreamSummary' smart constructor.
newtype ProgressUpdateStreamSummary = ProgressUpdateStreamSummary'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
    progressUpdateStreamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProgressUpdateStreamSummary' with the minimum fields required to make a request.
--
-- * 'progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in this field./
mkProgressUpdateStreamSummary ::
  ProgressUpdateStreamSummary
mkProgressUpdateStreamSummary =
  ProgressUpdateStreamSummary'
    { progressUpdateStreamName =
        Lude.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'progressUpdateStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pussProgressUpdateStreamName :: Lens.Lens' ProgressUpdateStreamSummary (Lude.Maybe Lude.Text)
pussProgressUpdateStreamName = Lens.lens (progressUpdateStreamName :: ProgressUpdateStreamSummary -> Lude.Maybe Lude.Text) (\s a -> s {progressUpdateStreamName = a} :: ProgressUpdateStreamSummary)
{-# DEPRECATED pussProgressUpdateStreamName "Use generic-lens or generic-optics with 'progressUpdateStreamName' instead." #-}

instance Lude.FromJSON ProgressUpdateStreamSummary where
  parseJSON =
    Lude.withObject
      "ProgressUpdateStreamSummary"
      ( \x ->
          ProgressUpdateStreamSummary'
            Lude.<$> (x Lude..:? "ProgressUpdateStreamName")
      )
