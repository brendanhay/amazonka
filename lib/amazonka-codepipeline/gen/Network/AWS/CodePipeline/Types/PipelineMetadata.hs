{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineMetadata
  ( PipelineMetadata (..),

    -- * Smart constructor
    mkPipelineMetadata,

    -- * Lenses
    pmCreated,
    pmPipelineARN,
    pmUpdated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a pipeline.
--
-- /See:/ 'mkPipelineMetadata' smart constructor.
data PipelineMetadata = PipelineMetadata'
  { -- | The date and time the pipeline was created, in timestamp format.
    created :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineARN :: Lude.Maybe Lude.Text,
    -- | The date and time the pipeline was last updated, in timestamp format.
    updated :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineMetadata' with the minimum fields required to make a request.
--
-- * 'created' - The date and time the pipeline was created, in timestamp format.
-- * 'pipelineARN' - The Amazon Resource Name (ARN) of the pipeline.
-- * 'updated' - The date and time the pipeline was last updated, in timestamp format.
mkPipelineMetadata ::
  PipelineMetadata
mkPipelineMetadata =
  PipelineMetadata'
    { created = Lude.Nothing,
      pipelineARN = Lude.Nothing,
      updated = Lude.Nothing
    }

-- | The date and time the pipeline was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmCreated :: Lens.Lens' PipelineMetadata (Lude.Maybe Lude.Timestamp)
pmCreated = Lens.lens (created :: PipelineMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: PipelineMetadata)
{-# DEPRECATED pmCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The Amazon Resource Name (ARN) of the pipeline.
--
-- /Note:/ Consider using 'pipelineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmPipelineARN :: Lens.Lens' PipelineMetadata (Lude.Maybe Lude.Text)
pmPipelineARN = Lens.lens (pipelineARN :: PipelineMetadata -> Lude.Maybe Lude.Text) (\s a -> s {pipelineARN = a} :: PipelineMetadata)
{-# DEPRECATED pmPipelineARN "Use generic-lens or generic-optics with 'pipelineARN' instead." #-}

-- | The date and time the pipeline was last updated, in timestamp format.
--
-- /Note:/ Consider using 'updated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmUpdated :: Lens.Lens' PipelineMetadata (Lude.Maybe Lude.Timestamp)
pmUpdated = Lens.lens (updated :: PipelineMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {updated = a} :: PipelineMetadata)
{-# DEPRECATED pmUpdated "Use generic-lens or generic-optics with 'updated' instead." #-}

instance Lude.FromJSON PipelineMetadata where
  parseJSON =
    Lude.withObject
      "PipelineMetadata"
      ( \x ->
          PipelineMetadata'
            Lude.<$> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "pipelineArn")
            Lude.<*> (x Lude..:? "updated")
      )
