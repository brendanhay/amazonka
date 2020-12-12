{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Output
  ( Output (..),

    -- * Smart constructor
    mkOutput,

    -- * Lenses
    oOutputValue,
    oOutputKey,
    oExportName,
    oDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Output data type.
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { outputValue :: Lude.Maybe Lude.Text,
    outputKey :: Lude.Maybe Lude.Text,
    exportName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- * 'description' - User defined description associated with the output.
-- * 'exportName' - The name of the export associated with the output.
-- * 'outputKey' - The key associated with the output.
-- * 'outputValue' - The value associated with the output.
mkOutput ::
  Output
mkOutput =
  Output'
    { outputValue = Lude.Nothing,
      outputKey = Lude.Nothing,
      exportName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The value associated with the output.
--
-- /Note:/ Consider using 'outputValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputValue :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oOutputValue = Lens.lens (outputValue :: Output -> Lude.Maybe Lude.Text) (\s a -> s {outputValue = a} :: Output)
{-# DEPRECATED oOutputValue "Use generic-lens or generic-optics with 'outputValue' instead." #-}

-- | The key associated with the output.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputKey :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oOutputKey = Lens.lens (outputKey :: Output -> Lude.Maybe Lude.Text) (\s a -> s {outputKey = a} :: Output)
{-# DEPRECATED oOutputKey "Use generic-lens or generic-optics with 'outputKey' instead." #-}

-- | The name of the export associated with the output.
--
-- /Note:/ Consider using 'exportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oExportName :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oExportName = Lens.lens (exportName :: Output -> Lude.Maybe Lude.Text) (\s a -> s {exportName = a} :: Output)
{-# DEPRECATED oExportName "Use generic-lens or generic-optics with 'exportName' instead." #-}

-- | User defined description associated with the output.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDescription :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oDescription = Lens.lens (description :: Output -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Output)
{-# DEPRECATED oDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML Output where
  parseXML x =
    Output'
      Lude.<$> (x Lude..@? "OutputValue")
      Lude.<*> (x Lude..@? "OutputKey")
      Lude.<*> (x Lude..@? "ExportName")
      Lude.<*> (x Lude..@? "Description")
