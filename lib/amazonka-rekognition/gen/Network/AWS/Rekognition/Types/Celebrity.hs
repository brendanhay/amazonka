{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Celebrity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Celebrity
  ( Celebrity (..),

    -- * Smart constructor
    mkCelebrity,

    -- * Lenses
    cMatchConfidence,
    cURLs,
    cName,
    cId,
    cFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.ComparedFace

-- | Provides information about a celebrity recognized by the 'RecognizeCelebrities' operation.
--
-- /See:/ 'mkCelebrity' smart constructor.
data Celebrity = Celebrity'
  { -- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
    matchConfidence :: Lude.Maybe Lude.Double,
    -- | An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
    urls :: Lude.Maybe [Lude.Text],
    -- | The name of the celebrity.
    name :: Lude.Maybe Lude.Text,
    -- | A unique identifier for the celebrity.
    id :: Lude.Maybe Lude.Text,
    -- | Provides information about the celebrity's face, such as its location on the image.
    face :: Lude.Maybe ComparedFace
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Celebrity' with the minimum fields required to make a request.
--
-- * 'matchConfidence' - The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
-- * 'urls' - An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
-- * 'name' - The name of the celebrity.
-- * 'id' - A unique identifier for the celebrity.
-- * 'face' - Provides information about the celebrity's face, such as its location on the image.
mkCelebrity ::
  Celebrity
mkCelebrity =
  Celebrity'
    { matchConfidence = Lude.Nothing,
      urls = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      face = Lude.Nothing
    }

-- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
--
-- /Note:/ Consider using 'matchConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMatchConfidence :: Lens.Lens' Celebrity (Lude.Maybe Lude.Double)
cMatchConfidence = Lens.lens (matchConfidence :: Celebrity -> Lude.Maybe Lude.Double) (\s a -> s {matchConfidence = a} :: Celebrity)
{-# DEPRECATED cMatchConfidence "Use generic-lens or generic-optics with 'matchConfidence' instead." #-}

-- | An array of URLs pointing to additional information about the celebrity. If there is no additional information about the celebrity, this list is empty.
--
-- /Note:/ Consider using 'urls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cURLs :: Lens.Lens' Celebrity (Lude.Maybe [Lude.Text])
cURLs = Lens.lens (urls :: Celebrity -> Lude.Maybe [Lude.Text]) (\s a -> s {urls = a} :: Celebrity)
{-# DEPRECATED cURLs "Use generic-lens or generic-optics with 'urls' instead." #-}

-- | The name of the celebrity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Celebrity (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Celebrity -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Celebrity)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for the celebrity.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Celebrity (Lude.Maybe Lude.Text)
cId = Lens.lens (id :: Celebrity -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Celebrity)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Provides information about the celebrity's face, such as its location on the image.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cFace :: Lens.Lens' Celebrity (Lude.Maybe ComparedFace)
cFace = Lens.lens (face :: Celebrity -> Lude.Maybe ComparedFace) (\s a -> s {face = a} :: Celebrity)
{-# DEPRECATED cFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON Celebrity where
  parseJSON =
    Lude.withObject
      "Celebrity"
      ( \x ->
          Celebrity'
            Lude.<$> (x Lude..:? "MatchConfidence")
            Lude.<*> (x Lude..:? "Urls" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Face")
      )
