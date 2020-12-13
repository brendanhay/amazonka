{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GrokClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GrokClassifier
  ( GrokClassifier (..),

    -- * Smart constructor
    mkGrokClassifier,

    -- * Lenses
    gcCreationTime,
    gcLastUpdated,
    gcClassification,
    gcName,
    gcVersion,
    gcCustomPatterns,
    gcGrokPattern,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A classifier that uses @grok@ patterns.
--
-- /See:/ 'mkGrokClassifier' smart constructor.
data GrokClassifier = GrokClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The time that this classifier was last updated.
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    -- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
    classification :: Lude.Text,
    -- | The name of the classifier.
    name :: Lude.Text,
    -- | The version of this classifier.
    version :: Lude.Maybe Lude.Integer,
    -- | Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
    customPatterns :: Lude.Maybe Lude.Text,
    -- | The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
    grokPattern :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GrokClassifier' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that this classifier was registered.
-- * 'lastUpdated' - The time that this classifier was last updated.
-- * 'classification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
-- * 'name' - The name of the classifier.
-- * 'version' - The version of this classifier.
-- * 'customPatterns' - Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
-- * 'grokPattern' - The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
mkGrokClassifier ::
  -- | 'classification'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'grokPattern'
  Lude.Text ->
  GrokClassifier
mkGrokClassifier pClassification_ pName_ pGrokPattern_ =
  GrokClassifier'
    { creationTime = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      classification = pClassification_,
      name = pName_,
      version = Lude.Nothing,
      customPatterns = Lude.Nothing,
      grokPattern = pGrokPattern_
    }

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCreationTime :: Lens.Lens' GrokClassifier (Lude.Maybe Lude.Timestamp)
gcCreationTime = Lens.lens (creationTime :: GrokClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: GrokClassifier)
{-# DEPRECATED gcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcLastUpdated :: Lens.Lens' GrokClassifier (Lude.Maybe Lude.Timestamp)
gcLastUpdated = Lens.lens (lastUpdated :: GrokClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: GrokClassifier)
{-# DEPRECATED gcLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClassification :: Lens.Lens' GrokClassifier Lude.Text
gcClassification = Lens.lens (classification :: GrokClassifier -> Lude.Text) (\s a -> s {classification = a} :: GrokClassifier)
{-# DEPRECATED gcClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcName :: Lens.Lens' GrokClassifier Lude.Text
gcName = Lens.lens (name :: GrokClassifier -> Lude.Text) (\s a -> s {name = a} :: GrokClassifier)
{-# DEPRECATED gcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcVersion :: Lens.Lens' GrokClassifier (Lude.Maybe Lude.Integer)
gcVersion = Lens.lens (version :: GrokClassifier -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: GrokClassifier)
{-# DEPRECATED gcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
--
-- /Note:/ Consider using 'customPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCustomPatterns :: Lens.Lens' GrokClassifier (Lude.Maybe Lude.Text)
gcCustomPatterns = Lens.lens (customPatterns :: GrokClassifier -> Lude.Maybe Lude.Text) (\s a -> s {customPatterns = a} :: GrokClassifier)
{-# DEPRECATED gcCustomPatterns "Use generic-lens or generic-optics with 'customPatterns' instead." #-}

-- | The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
--
-- /Note:/ Consider using 'grokPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGrokPattern :: Lens.Lens' GrokClassifier Lude.Text
gcGrokPattern = Lens.lens (grokPattern :: GrokClassifier -> Lude.Text) (\s a -> s {grokPattern = a} :: GrokClassifier)
{-# DEPRECATED gcGrokPattern "Use generic-lens or generic-optics with 'grokPattern' instead." #-}

instance Lude.FromJSON GrokClassifier where
  parseJSON =
    Lude.withObject
      "GrokClassifier"
      ( \x ->
          GrokClassifier'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..: "Classification")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "CustomPatterns")
            Lude.<*> (x Lude..: "GrokPattern")
      )
