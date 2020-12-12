{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.PipelineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.PipelineConfig
  ( PipelineConfig (..),

    -- * Smart constructor
    mkPipelineConfig,

    -- * Lenses
    pcFunctions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The pipeline configuration for a resolver of kind @PIPELINE@ .
--
-- /See:/ 'mkPipelineConfig' smart constructor.
newtype PipelineConfig = PipelineConfig'
  { functions ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PipelineConfig' with the minimum fields required to make a request.
--
-- * 'functions' - A list of @Function@ objects.
mkPipelineConfig ::
  PipelineConfig
mkPipelineConfig = PipelineConfig' {functions = Lude.Nothing}

-- | A list of @Function@ objects.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcFunctions :: Lens.Lens' PipelineConfig (Lude.Maybe [Lude.Text])
pcFunctions = Lens.lens (functions :: PipelineConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {functions = a} :: PipelineConfig)
{-# DEPRECATED pcFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

instance Lude.FromJSON PipelineConfig where
  parseJSON =
    Lude.withObject
      "PipelineConfig"
      ( \x ->
          PipelineConfig'
            Lude.<$> (x Lude..:? "functions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PipelineConfig where
  toJSON PipelineConfig' {..} =
    Lude.object
      (Lude.catMaybes [("functions" Lude..=) Lude.<$> functions])
