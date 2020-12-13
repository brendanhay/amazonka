{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortConfig
  ( AbortConfig (..),

    -- * Smart constructor
    mkAbortConfig,

    -- * Lenses
    acCriteriaList,
  )
where

import Network.AWS.IoT.Types.AbortCriteria
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAbortConfig' smart constructor.
newtype AbortConfig = AbortConfig'
  { -- | The list of criteria that determine when and how to abort the job.
    criteriaList :: Lude.NonEmpty AbortCriteria
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortConfig' with the minimum fields required to make a request.
--
-- * 'criteriaList' - The list of criteria that determine when and how to abort the job.
mkAbortConfig ::
  -- | 'criteriaList'
  Lude.NonEmpty AbortCriteria ->
  AbortConfig
mkAbortConfig pCriteriaList_ =
  AbortConfig' {criteriaList = pCriteriaList_}

-- | The list of criteria that determine when and how to abort the job.
--
-- /Note:/ Consider using 'criteriaList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acCriteriaList :: Lens.Lens' AbortConfig (Lude.NonEmpty AbortCriteria)
acCriteriaList = Lens.lens (criteriaList :: AbortConfig -> Lude.NonEmpty AbortCriteria) (\s a -> s {criteriaList = a} :: AbortConfig)
{-# DEPRECATED acCriteriaList "Use generic-lens or generic-optics with 'criteriaList' instead." #-}

instance Lude.FromJSON AbortConfig where
  parseJSON =
    Lude.withObject
      "AbortConfig"
      (\x -> AbortConfig' Lude.<$> (x Lude..: "criteriaList"))

instance Lude.ToJSON AbortConfig where
  toJSON AbortConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("criteriaList" Lude..= criteriaList)])
