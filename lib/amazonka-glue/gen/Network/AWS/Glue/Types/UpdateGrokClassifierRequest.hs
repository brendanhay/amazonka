{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateGrokClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateGrokClassifierRequest
  ( UpdateGrokClassifierRequest (..),

    -- * Smart constructor
    mkUpdateGrokClassifierRequest,

    -- * Lenses
    ugcrClassification,
    ugcrName,
    ugcrCustomPatterns,
    ugcrGrokPattern,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a grok classifier to update when passed to @UpdateClassifier@ .
--
-- /See:/ 'mkUpdateGrokClassifierRequest' smart constructor.
data UpdateGrokClassifierRequest = UpdateGrokClassifierRequest'
  { -- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
    classification :: Lude.Maybe Lude.Text,
    -- | The name of the @GrokClassifier@ .
    name :: Lude.Text,
    -- | Optional custom grok patterns used by this classifier.
    customPatterns :: Lude.Maybe Lude.Text,
    -- | The grok pattern used by this classifier.
    grokPattern :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGrokClassifierRequest' with the minimum fields required to make a request.
--
-- * 'classification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
-- * 'name' - The name of the @GrokClassifier@ .
-- * 'customPatterns' - Optional custom grok patterns used by this classifier.
-- * 'grokPattern' - The grok pattern used by this classifier.
mkUpdateGrokClassifierRequest ::
  -- | 'name'
  Lude.Text ->
  UpdateGrokClassifierRequest
mkUpdateGrokClassifierRequest pName_ =
  UpdateGrokClassifierRequest'
    { classification = Lude.Nothing,
      name = pName_,
      customPatterns = Lude.Nothing,
      grokPattern = Lude.Nothing
    }

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrClassification :: Lens.Lens' UpdateGrokClassifierRequest (Lude.Maybe Lude.Text)
ugcrClassification = Lens.lens (classification :: UpdateGrokClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {classification = a} :: UpdateGrokClassifierRequest)
{-# DEPRECATED ugcrClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The name of the @GrokClassifier@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrName :: Lens.Lens' UpdateGrokClassifierRequest Lude.Text
ugcrName = Lens.lens (name :: UpdateGrokClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: UpdateGrokClassifierRequest)
{-# DEPRECATED ugcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional custom grok patterns used by this classifier.
--
-- /Note:/ Consider using 'customPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrCustomPatterns :: Lens.Lens' UpdateGrokClassifierRequest (Lude.Maybe Lude.Text)
ugcrCustomPatterns = Lens.lens (customPatterns :: UpdateGrokClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {customPatterns = a} :: UpdateGrokClassifierRequest)
{-# DEPRECATED ugcrCustomPatterns "Use generic-lens or generic-optics with 'customPatterns' instead." #-}

-- | The grok pattern used by this classifier.
--
-- /Note:/ Consider using 'grokPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrGrokPattern :: Lens.Lens' UpdateGrokClassifierRequest (Lude.Maybe Lude.Text)
ugcrGrokPattern = Lens.lens (grokPattern :: UpdateGrokClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {grokPattern = a} :: UpdateGrokClassifierRequest)
{-# DEPRECATED ugcrGrokPattern "Use generic-lens or generic-optics with 'grokPattern' instead." #-}

instance Lude.ToJSON UpdateGrokClassifierRequest where
  toJSON UpdateGrokClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Classification" Lude..=) Lude.<$> classification,
            Lude.Just ("Name" Lude..= name),
            ("CustomPatterns" Lude..=) Lude.<$> customPatterns,
            ("GrokPattern" Lude..=) Lude.<$> grokPattern
          ]
      )
