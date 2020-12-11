-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessingConfiguration
  ( ProcessingConfiguration (..),

    -- * Smart constructor
    mkProcessingConfiguration,

    -- * Lenses
    pcEnabled,
    pcProcessors,
  )
where

import Network.AWS.Firehose.Types.Processor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a data processing configuration.
--
-- /See:/ 'mkProcessingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { enabled ::
      Lude.Maybe Lude.Bool,
    processors :: Lude.Maybe [Processor]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingConfiguration' with the minimum fields required to make a request.
--
-- * 'enabled' - Enables or disables data processing.
-- * 'processors' - The data processors.
mkProcessingConfiguration ::
  ProcessingConfiguration
mkProcessingConfiguration =
  ProcessingConfiguration'
    { enabled = Lude.Nothing,
      processors = Lude.Nothing
    }

-- | Enables or disables data processing.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcEnabled :: Lens.Lens' ProcessingConfiguration (Lude.Maybe Lude.Bool)
pcEnabled = Lens.lens (enabled :: ProcessingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ProcessingConfiguration)
{-# DEPRECATED pcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The data processors.
--
-- /Note:/ Consider using 'processors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProcessors :: Lens.Lens' ProcessingConfiguration (Lude.Maybe [Processor])
pcProcessors = Lens.lens (processors :: ProcessingConfiguration -> Lude.Maybe [Processor]) (\s a -> s {processors = a} :: ProcessingConfiguration)
{-# DEPRECATED pcProcessors "Use generic-lens or generic-optics with 'processors' instead." #-}

instance Lude.FromJSON ProcessingConfiguration where
  parseJSON =
    Lude.withObject
      "ProcessingConfiguration"
      ( \x ->
          ProcessingConfiguration'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "Processors" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ProcessingConfiguration where
  toJSON ProcessingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("Processors" Lude..=) Lude.<$> processors
          ]
      )
