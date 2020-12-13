{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentDefaultVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentDefaultVersionDescription
  ( DocumentDefaultVersionDescription (..),

    -- * Smart constructor
    mkDocumentDefaultVersionDescription,

    -- * Lenses
    ddvdDefaultVersionName,
    ddvdDefaultVersion,
    ddvdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A default version of a document.
--
-- /See:/ 'mkDocumentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
  { -- | The default version of the artifact associated with the document.
    defaultVersionName :: Lude.Maybe Lude.Text,
    -- | The default version of the document.
    defaultVersion :: Lude.Maybe Lude.Text,
    -- | The name of the document.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentDefaultVersionDescription' with the minimum fields required to make a request.
--
-- * 'defaultVersionName' - The default version of the artifact associated with the document.
-- * 'defaultVersion' - The default version of the document.
-- * 'name' - The name of the document.
mkDocumentDefaultVersionDescription ::
  DocumentDefaultVersionDescription
mkDocumentDefaultVersionDescription =
  DocumentDefaultVersionDescription'
    { defaultVersionName =
        Lude.Nothing,
      defaultVersion = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The default version of the artifact associated with the document.
--
-- /Note:/ Consider using 'defaultVersionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvdDefaultVersionName :: Lens.Lens' DocumentDefaultVersionDescription (Lude.Maybe Lude.Text)
ddvdDefaultVersionName = Lens.lens (defaultVersionName :: DocumentDefaultVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersionName = a} :: DocumentDefaultVersionDescription)
{-# DEPRECATED ddvdDefaultVersionName "Use generic-lens or generic-optics with 'defaultVersionName' instead." #-}

-- | The default version of the document.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvdDefaultVersion :: Lens.Lens' DocumentDefaultVersionDescription (Lude.Maybe Lude.Text)
ddvdDefaultVersion = Lens.lens (defaultVersion :: DocumentDefaultVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersion = a} :: DocumentDefaultVersionDescription)
{-# DEPRECATED ddvdDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvdName :: Lens.Lens' DocumentDefaultVersionDescription (Lude.Maybe Lude.Text)
ddvdName = Lens.lens (name :: DocumentDefaultVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentDefaultVersionDescription)
{-# DEPRECATED ddvdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON DocumentDefaultVersionDescription where
  parseJSON =
    Lude.withObject
      "DocumentDefaultVersionDescription"
      ( \x ->
          DocumentDefaultVersionDescription'
            Lude.<$> (x Lude..:? "DefaultVersionName")
            Lude.<*> (x Lude..:? "DefaultVersion")
            Lude.<*> (x Lude..:? "Name")
      )
