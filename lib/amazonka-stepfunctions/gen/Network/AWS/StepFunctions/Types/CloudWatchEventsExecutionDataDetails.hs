{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
  ( CloudWatchEventsExecutionDataDetails (..),

    -- * Smart constructor
    mkCloudWatchEventsExecutionDataDetails,

    -- * Lenses
    cweeddIncluded,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides details about execution input or output.
--
-- /See:/ 'mkCloudWatchEventsExecutionDataDetails' smart constructor.
newtype CloudWatchEventsExecutionDataDetails = CloudWatchEventsExecutionDataDetails'
  { -- | Indicates whether input or output was included in the response. Always @true@ for API calls.
    included :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchEventsExecutionDataDetails' with the minimum fields required to make a request.
--
-- * 'included' - Indicates whether input or output was included in the response. Always @true@ for API calls.
mkCloudWatchEventsExecutionDataDetails ::
  CloudWatchEventsExecutionDataDetails
mkCloudWatchEventsExecutionDataDetails =
  CloudWatchEventsExecutionDataDetails' {included = Lude.Nothing}

-- | Indicates whether input or output was included in the response. Always @true@ for API calls.
--
-- /Note:/ Consider using 'included' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweeddIncluded :: Lens.Lens' CloudWatchEventsExecutionDataDetails (Lude.Maybe Lude.Bool)
cweeddIncluded = Lens.lens (included :: CloudWatchEventsExecutionDataDetails -> Lude.Maybe Lude.Bool) (\s a -> s {included = a} :: CloudWatchEventsExecutionDataDetails)
{-# DEPRECATED cweeddIncluded "Use generic-lens or generic-optics with 'included' instead." #-}

instance Lude.FromJSON CloudWatchEventsExecutionDataDetails where
  parseJSON =
    Lude.withObject
      "CloudWatchEventsExecutionDataDetails"
      ( \x ->
          CloudWatchEventsExecutionDataDetails'
            Lude.<$> (x Lude..:? "included")
      )
