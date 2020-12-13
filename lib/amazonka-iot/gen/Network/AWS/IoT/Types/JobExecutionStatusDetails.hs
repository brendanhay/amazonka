{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionStatusDetails
  ( JobExecutionStatusDetails (..),

    -- * Smart constructor
    mkJobExecutionStatusDetails,

    -- * Lenses
    jesdDetailsMap,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the job execution status.
--
-- /See:/ 'mkJobExecutionStatusDetails' smart constructor.
newtype JobExecutionStatusDetails = JobExecutionStatusDetails'
  { -- | The job execution status.
    detailsMap :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionStatusDetails' with the minimum fields required to make a request.
--
-- * 'detailsMap' - The job execution status.
mkJobExecutionStatusDetails ::
  JobExecutionStatusDetails
mkJobExecutionStatusDetails =
  JobExecutionStatusDetails' {detailsMap = Lude.Nothing}

-- | The job execution status.
--
-- /Note:/ Consider using 'detailsMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesdDetailsMap :: Lens.Lens' JobExecutionStatusDetails (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jesdDetailsMap = Lens.lens (detailsMap :: JobExecutionStatusDetails -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {detailsMap = a} :: JobExecutionStatusDetails)
{-# DEPRECATED jesdDetailsMap "Use generic-lens or generic-optics with 'detailsMap' instead." #-}

instance Lude.FromJSON JobExecutionStatusDetails where
  parseJSON =
    Lude.withObject
      "JobExecutionStatusDetails"
      ( \x ->
          JobExecutionStatusDetails'
            Lude.<$> (x Lude..:? "detailsMap" Lude..!= Lude.mempty)
      )
