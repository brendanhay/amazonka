{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobAbortConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobAbortConfig
  ( AwsJobAbortConfig (..)
  -- * Smart constructor
  , mkAwsJobAbortConfig
  -- * Lenses
  , ajacAbortCriteriaList
  ) where

import qualified Network.AWS.IoT.Types.AwsJobAbortCriteria as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAwsJobAbortConfig' smart constructor.
newtype AwsJobAbortConfig = AwsJobAbortConfig'
  { abortCriteriaList :: Core.NonEmpty Types.AwsJobAbortCriteria
    -- ^ The list of criteria that determine when and how to abort the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AwsJobAbortConfig' value with any optional fields omitted.
mkAwsJobAbortConfig
    :: Core.NonEmpty Types.AwsJobAbortCriteria -- ^ 'abortCriteriaList'
    -> AwsJobAbortConfig
mkAwsJobAbortConfig abortCriteriaList
  = AwsJobAbortConfig'{abortCriteriaList}

-- | The list of criteria that determine when and how to abort the job.
--
-- /Note:/ Consider using 'abortCriteriaList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacAbortCriteriaList :: Lens.Lens' AwsJobAbortConfig (Core.NonEmpty Types.AwsJobAbortCriteria)
ajacAbortCriteriaList = Lens.field @"abortCriteriaList"
{-# INLINEABLE ajacAbortCriteriaList #-}
{-# DEPRECATED abortCriteriaList "Use generic-lens or generic-optics with 'abortCriteriaList' instead"  #-}

instance Core.FromJSON AwsJobAbortConfig where
        toJSON AwsJobAbortConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("abortCriteriaList" Core..= abortCriteriaList)])
