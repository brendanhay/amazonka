{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OutputSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OutputSource
  ( OutputSource (..),

    -- * Smart constructor
    mkOutputSource,

    -- * Lenses
    osOutputSourceId,
    osOutputSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the source where the association execution details are stored.
--
-- /See:/ 'mkOutputSource' smart constructor.
data OutputSource = OutputSource'
  { -- | The ID of the output source, for example the URL of an S3 bucket.
    outputSourceId :: Lude.Maybe Lude.Text,
    -- | The type of source where the association execution details are stored, for example, Amazon S3.
    outputSourceType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputSource' with the minimum fields required to make a request.
--
-- * 'outputSourceId' - The ID of the output source, for example the URL of an S3 bucket.
-- * 'outputSourceType' - The type of source where the association execution details are stored, for example, Amazon S3.
mkOutputSource ::
  OutputSource
mkOutputSource =
  OutputSource'
    { outputSourceId = Lude.Nothing,
      outputSourceType = Lude.Nothing
    }

-- | The ID of the output source, for example the URL of an S3 bucket.
--
-- /Note:/ Consider using 'outputSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOutputSourceId :: Lens.Lens' OutputSource (Lude.Maybe Lude.Text)
osOutputSourceId = Lens.lens (outputSourceId :: OutputSource -> Lude.Maybe Lude.Text) (\s a -> s {outputSourceId = a} :: OutputSource)
{-# DEPRECATED osOutputSourceId "Use generic-lens or generic-optics with 'outputSourceId' instead." #-}

-- | The type of source where the association execution details are stored, for example, Amazon S3.
--
-- /Note:/ Consider using 'outputSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOutputSourceType :: Lens.Lens' OutputSource (Lude.Maybe Lude.Text)
osOutputSourceType = Lens.lens (outputSourceType :: OutputSource -> Lude.Maybe Lude.Text) (\s a -> s {outputSourceType = a} :: OutputSource)
{-# DEPRECATED osOutputSourceType "Use generic-lens or generic-optics with 'outputSourceType' instead." #-}

instance Lude.FromJSON OutputSource where
  parseJSON =
    Lude.withObject
      "OutputSource"
      ( \x ->
          OutputSource'
            Lude.<$> (x Lude..:? "OutputSourceId")
            Lude.<*> (x Lude..:? "OutputSourceType")
      )
