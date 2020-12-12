{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineSummary
  ( PipelineSummary (..),

    -- * Smart constructor
    mkPipelineSummary,

    -- * Lenses
    psCreated,
    psName,
    psVersion,
    psUpdated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns a summary of a pipeline.
--
-- /See:/ 'mkPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { created ::
      Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Natural,
    updated :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineSummary' with the minimum fields required to make a request.
--
-- * 'created' - The date and time the pipeline was created, in timestamp format.
-- * 'name' - The name of the pipeline.
-- * 'updated' - The date and time of the last update to the pipeline, in timestamp format.
-- * 'version' - The version number of the pipeline.
mkPipelineSummary ::
  PipelineSummary
mkPipelineSummary =
  PipelineSummary'
    { created = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      updated = Lude.Nothing
    }

-- | The date and time the pipeline was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCreated :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Timestamp)
psCreated = Lens.lens (created :: PipelineSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: PipelineSummary)
{-# DEPRECATED psCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Text)
psName = Lens.lens (name :: PipelineSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PipelineSummary)
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the pipeline.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psVersion :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Natural)
psVersion = Lens.lens (version :: PipelineSummary -> Lude.Maybe Lude.Natural) (\s a -> s {version = a} :: PipelineSummary)
{-# DEPRECATED psVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date and time of the last update to the pipeline, in timestamp format.
--
-- /Note:/ Consider using 'updated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psUpdated :: Lens.Lens' PipelineSummary (Lude.Maybe Lude.Timestamp)
psUpdated = Lens.lens (updated :: PipelineSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {updated = a} :: PipelineSummary)
{-# DEPRECATED psUpdated "Use generic-lens or generic-optics with 'updated' instead." #-}

instance Lude.FromJSON PipelineSummary where
  parseJSON =
    Lude.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Lude.<$> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "updated")
      )
