-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchFilter
  ( BuildBatchFilter (..),

    -- * Smart constructor
    mkBuildBatchFilter,

    -- * Lenses
    bbfStatus,
  )
where

import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies filters when retrieving batch builds.
--
-- /See:/ 'mkBuildBatchFilter' smart constructor.
newtype BuildBatchFilter = BuildBatchFilter'
  { status ::
      Lude.Maybe StatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildBatchFilter' with the minimum fields required to make a request.
--
-- * 'status' - The status of the batch builds to retrieve. Only batch builds that have this status will be retrieved.
mkBuildBatchFilter ::
  BuildBatchFilter
mkBuildBatchFilter = BuildBatchFilter' {status = Lude.Nothing}

-- | The status of the batch builds to retrieve. Only batch builds that have this status will be retrieved.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbfStatus :: Lens.Lens' BuildBatchFilter (Lude.Maybe StatusType)
bbfStatus = Lens.lens (status :: BuildBatchFilter -> Lude.Maybe StatusType) (\s a -> s {status = a} :: BuildBatchFilter)
{-# DEPRECATED bbfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToJSON BuildBatchFilter where
  toJSON BuildBatchFilter' {..} =
    Lude.object (Lude.catMaybes [("status" Lude..=) Lude.<$> status])
