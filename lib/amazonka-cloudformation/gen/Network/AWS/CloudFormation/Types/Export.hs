-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Export
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Export
  ( Export (..),

    -- * Smart constructor
    mkExport,

    -- * Lenses
    eValue,
    eExportingStackId,
    eName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Export@ structure describes the exported output values for a stack.
--
-- /See:/ 'mkExport' smart constructor.
data Export = Export'
  { value :: Lude.Maybe Lude.Text,
    exportingStackId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Export' with the minimum fields required to make a request.
--
-- * 'exportingStackId' - The stack that contains the exported output name and value.
-- * 'name' - The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
-- * 'value' - The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
mkExport ::
  Export
mkExport =
  Export'
    { value = Lude.Nothing,
      exportingStackId = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eValue :: Lens.Lens' Export (Lude.Maybe Lude.Text)
eValue = Lens.lens (value :: Export -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Export)
{-# DEPRECATED eValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The stack that contains the exported output name and value.
--
-- /Note:/ Consider using 'exportingStackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExportingStackId :: Lens.Lens' Export (Lude.Maybe Lude.Text)
eExportingStackId = Lens.lens (exportingStackId :: Export -> Lude.Maybe Lude.Text) (\s a -> s {exportingStackId = a} :: Export)
{-# DEPRECATED eExportingStackId "Use generic-lens or generic-optics with 'exportingStackId' instead." #-}

-- | The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eName :: Lens.Lens' Export (Lude.Maybe Lude.Text)
eName = Lens.lens (name :: Export -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Export)
{-# DEPRECATED eName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML Export where
  parseXML x =
    Export'
      Lude.<$> (x Lude..@? "Value")
      Lude.<*> (x Lude..@? "ExportingStackId")
      Lude.<*> (x Lude..@? "Name")
