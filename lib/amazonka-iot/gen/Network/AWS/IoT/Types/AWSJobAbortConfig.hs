-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortConfig
  ( AWSJobAbortConfig (..),

    -- * Smart constructor
    mkAWSJobAbortConfig,

    -- * Lenses
    ajacAbortCriteriaList,
  )
where

import Network.AWS.IoT.Types.AWSJobAbortCriteria
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAWSJobAbortConfig' smart constructor.
newtype AWSJobAbortConfig = AWSJobAbortConfig'
  { abortCriteriaList ::
      Lude.NonEmpty AWSJobAbortCriteria
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSJobAbortConfig' with the minimum fields required to make a request.
--
-- * 'abortCriteriaList' - The list of criteria that determine when and how to abort the job.
mkAWSJobAbortConfig ::
  -- | 'abortCriteriaList'
  Lude.NonEmpty AWSJobAbortCriteria ->
  AWSJobAbortConfig
mkAWSJobAbortConfig pAbortCriteriaList_ =
  AWSJobAbortConfig' {abortCriteriaList = pAbortCriteriaList_}

-- | The list of criteria that determine when and how to abort the job.
--
-- /Note:/ Consider using 'abortCriteriaList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajacAbortCriteriaList :: Lens.Lens' AWSJobAbortConfig (Lude.NonEmpty AWSJobAbortCriteria)
ajacAbortCriteriaList = Lens.lens (abortCriteriaList :: AWSJobAbortConfig -> Lude.NonEmpty AWSJobAbortCriteria) (\s a -> s {abortCriteriaList = a} :: AWSJobAbortConfig)
{-# DEPRECATED ajacAbortCriteriaList "Use generic-lens or generic-optics with 'abortCriteriaList' instead." #-}

instance Lude.ToJSON AWSJobAbortConfig where
  toJSON AWSJobAbortConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("abortCriteriaList" Lude..= abortCriteriaList)]
      )
