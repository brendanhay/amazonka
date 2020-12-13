{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    drName,
    drVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An SSM document required by the current document.
--
-- /See:/ 'mkDocumentRequires' smart constructor.
data DocumentRequires = DocumentRequires'
  { -- | The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
    name :: Lude.Text,
    -- | The document version required by the current document.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
  DocumentRequires' {name = pName_, version = Lude.Nothing}

-- | The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drName :: Lens.Lens' DocumentRequires Lude.Text
drName = Lens.lens (name :: DocumentRequires -> Lude.Text) (\s a -> s {name = a} :: DocumentRequires)
{-# DEPRECATED drName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The document version required by the current document.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drVersion :: Lens.Lens' DocumentRequires (Lude.Maybe Lude.Text)
drVersion = Lens.lens (version :: DocumentRequires -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DocumentRequires)
{-# DEPRECATED drVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON DocumentRequires where
  parseJSON =
    Lude.withObject
      "DocumentRequires"
      ( \x ->
          DocumentRequires'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..:? "Version")
      )

instance Lude.ToJSON DocumentRequires where
  toJSON DocumentRequires' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("Version" Lude..=) Lude.<$> version
          ]
      )
