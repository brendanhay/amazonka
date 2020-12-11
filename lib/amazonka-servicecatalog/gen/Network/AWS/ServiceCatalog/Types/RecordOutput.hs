-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordOutput
  ( RecordOutput (..),

    -- * Smart constructor
    mkRecordOutput,

    -- * Lenses
    roOutputValue,
    roOutputKey,
    roDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
--
-- /See:/ 'mkRecordOutput' smart constructor.
data RecordOutput = RecordOutput'
  { outputValue ::
      Lude.Maybe Lude.Text,
    outputKey :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordOutput' with the minimum fields required to make a request.
--
-- * 'description' - The description of the output.
-- * 'outputKey' - The output key.
-- * 'outputValue' - The output value.
mkRecordOutput ::
  RecordOutput
mkRecordOutput =
  RecordOutput'
    { outputValue = Lude.Nothing,
      outputKey = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The output value.
--
-- /Note:/ Consider using 'outputValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roOutputValue :: Lens.Lens' RecordOutput (Lude.Maybe Lude.Text)
roOutputValue = Lens.lens (outputValue :: RecordOutput -> Lude.Maybe Lude.Text) (\s a -> s {outputValue = a} :: RecordOutput)
{-# DEPRECATED roOutputValue "Use generic-lens or generic-optics with 'outputValue' instead." #-}

-- | The output key.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roOutputKey :: Lens.Lens' RecordOutput (Lude.Maybe Lude.Text)
roOutputKey = Lens.lens (outputKey :: RecordOutput -> Lude.Maybe Lude.Text) (\s a -> s {outputKey = a} :: RecordOutput)
{-# DEPRECATED roOutputKey "Use generic-lens or generic-optics with 'outputKey' instead." #-}

-- | The description of the output.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roDescription :: Lens.Lens' RecordOutput (Lude.Maybe Lude.Text)
roDescription = Lens.lens (description :: RecordOutput -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RecordOutput)
{-# DEPRECATED roDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON RecordOutput where
  parseJSON =
    Lude.withObject
      "RecordOutput"
      ( \x ->
          RecordOutput'
            Lude.<$> (x Lude..:? "OutputValue")
            Lude.<*> (x Lude..:? "OutputKey")
            Lude.<*> (x Lude..:? "Description")
      )
