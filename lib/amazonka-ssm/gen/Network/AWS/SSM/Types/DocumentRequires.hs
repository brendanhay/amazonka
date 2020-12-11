-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentRequires
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentRequires
  ( DocumentRequires (..),

    -- * Smart constructor
    mkDocumentRequires,

    -- * Lenses
    drVersion,
    drName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An SSM document required by the current document.
--
-- /See:/ 'mkDocumentRequires' smart constructor.
data DocumentRequires = DocumentRequires'
  { version ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentRequires' with the minimum fields required to make a request.
--
-- * 'name' - The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
-- * 'version' - The document version required by the current document.
mkDocumentRequires ::
  -- | 'name'
  Lude.Text ->
  DocumentRequires
mkDocumentRequires pName_ =
  DocumentRequires' {version = Lude.Nothing, name = pName_}

-- | The document version required by the current document.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drVersion :: Lens.Lens' DocumentRequires (Lude.Maybe Lude.Text)
drVersion = Lens.lens (version :: DocumentRequires -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DocumentRequires)
{-# DEPRECATED drVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drName :: Lens.Lens' DocumentRequires Lude.Text
drName = Lens.lens (name :: DocumentRequires -> Lude.Text) (\s a -> s {name = a} :: DocumentRequires)
{-# DEPRECATED drName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON DocumentRequires where
  parseJSON =
    Lude.withObject
      "DocumentRequires"
      ( \x ->
          DocumentRequires'
            Lude.<$> (x Lude..:? "Version") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON DocumentRequires where
  toJSON DocumentRequires' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Version" Lude..=) Lude.<$> version,
            Lude.Just ("Name" Lude..= name)
          ]
      )
