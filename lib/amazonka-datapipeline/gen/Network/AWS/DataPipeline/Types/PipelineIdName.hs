{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineIdName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineIdName
  ( PipelineIdName (..),

    -- * Smart constructor
    mkPipelineIdName,

    -- * Lenses
    pinName,
    pinId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the name and identifier of a pipeline.
--
-- /See:/ 'mkPipelineIdName' smart constructor.
data PipelineIdName = PipelineIdName'
  { -- | The name of the pipeline.
    name :: Lude.Maybe Lude.Text,
    -- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineIdName' with the minimum fields required to make a request.
--
-- * 'name' - The name of the pipeline.
-- * 'id' - The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
mkPipelineIdName ::
  PipelineIdName
mkPipelineIdName =
  PipelineIdName' {name = Lude.Nothing, id = Lude.Nothing}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pinName :: Lens.Lens' PipelineIdName (Lude.Maybe Lude.Text)
pinName = Lens.lens (name :: PipelineIdName -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PipelineIdName)
{-# DEPRECATED pinName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pinId :: Lens.Lens' PipelineIdName (Lude.Maybe Lude.Text)
pinId = Lens.lens (id :: PipelineIdName -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PipelineIdName)
{-# DEPRECATED pinId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON PipelineIdName where
  parseJSON =
    Lude.withObject
      "PipelineIdName"
      ( \x ->
          PipelineIdName'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "id")
      )
