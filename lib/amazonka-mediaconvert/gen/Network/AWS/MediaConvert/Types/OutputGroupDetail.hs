{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupDetail
  ( OutputGroupDetail (..),

    -- * Smart constructor
    mkOutputGroupDetail,

    -- * Lenses
    ogdOutputDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.OutputDetail
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the output groups specified in the job settings.
--
-- /See:/ 'mkOutputGroupDetail' smart constructor.
newtype OutputGroupDetail = OutputGroupDetail'
  { -- | Details about the output
    outputDetails :: Lude.Maybe [OutputDetail]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputGroupDetail' with the minimum fields required to make a request.
--
-- * 'outputDetails' - Details about the output
mkOutputGroupDetail ::
  OutputGroupDetail
mkOutputGroupDetail =
  OutputGroupDetail' {outputDetails = Lude.Nothing}

-- | Details about the output
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogdOutputDetails :: Lens.Lens' OutputGroupDetail (Lude.Maybe [OutputDetail])
ogdOutputDetails = Lens.lens (outputDetails :: OutputGroupDetail -> Lude.Maybe [OutputDetail]) (\s a -> s {outputDetails = a} :: OutputGroupDetail)
{-# DEPRECATED ogdOutputDetails "Use generic-lens or generic-optics with 'outputDetails' instead." #-}

instance Lude.FromJSON OutputGroupDetail where
  parseJSON =
    Lude.withObject
      "OutputGroupDetail"
      ( \x ->
          OutputGroupDetail'
            Lude.<$> (x Lude..:? "outputDetails" Lude..!= Lude.mempty)
      )
