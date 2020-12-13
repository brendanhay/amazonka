{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Processor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Processor
  ( Processor (..),

    -- * Smart constructor
    mkProcessor,

    -- * Lenses
    pParameters,
    pType,
  )
where

import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.ProcessorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a data processor.
--
-- /See:/ 'mkProcessor' smart constructor.
data Processor = Processor'
  { -- | The processor parameters.
    parameters :: Lude.Maybe [ProcessorParameter],
    -- | The type of processor.
    type' :: ProcessorType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Processor' with the minimum fields required to make a request.
--
-- * 'parameters' - The processor parameters.
-- * 'type'' - The type of processor.
mkProcessor ::
  -- | 'type''
  ProcessorType ->
  Processor
mkProcessor pType_ =
  Processor' {parameters = Lude.Nothing, type' = pType_}

-- | The processor parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameters :: Lens.Lens' Processor (Lude.Maybe [ProcessorParameter])
pParameters = Lens.lens (parameters :: Processor -> Lude.Maybe [ProcessorParameter]) (\s a -> s {parameters = a} :: Processor)
{-# DEPRECATED pParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of processor.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Processor ProcessorType
pType = Lens.lens (type' :: Processor -> ProcessorType) (\s a -> s {type' = a} :: Processor)
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Processor where
  parseJSON =
    Lude.withObject
      "Processor"
      ( \x ->
          Processor'
            Lude.<$> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON Processor where
  toJSON Processor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Type" Lude..= type')
          ]
      )
