{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateGrokClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateGrokClassifierRequest
  ( CreateGrokClassifierRequest (..),

    -- * Smart constructor
    mkCreateGrokClassifierRequest,

    -- * Lenses
    cgcrClassification,
    cgcrName,
    cgcrCustomPatterns,
    cgcrGrokPattern,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a @grok@ classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateGrokClassifierRequest' smart constructor.
data CreateGrokClassifierRequest = CreateGrokClassifierRequest'
  { -- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
    classification :: Lude.Text,
    -- | The name of the new classifier.
    name :: Lude.Text,
    -- | Optional custom grok patterns used by this classifier.
    customPatterns :: Lude.Maybe Lude.Text,
    -- | The grok pattern used by this classifier.
    grokPattern :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGrokClassifierRequest' with the minimum fields required to make a request.
--
-- * 'classification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
-- * 'name' - The name of the new classifier.
-- * 'customPatterns' - Optional custom grok patterns used by this classifier.
-- * 'grokPattern' - The grok pattern used by this classifier.
mkCreateGrokClassifierRequest ::
  -- | 'classification'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'grokPattern'
  Lude.Text ->
  CreateGrokClassifierRequest
mkCreateGrokClassifierRequest pClassification_ pName_ pGrokPattern_ =
  CreateGrokClassifierRequest'
    { classification = pClassification_,
      name = pName_,
      customPatterns = Lude.Nothing,
      grokPattern = pGrokPattern_
    }

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrClassification :: Lens.Lens' CreateGrokClassifierRequest Lude.Text
cgcrClassification = Lens.lens (classification :: CreateGrokClassifierRequest -> Lude.Text) (\s a -> s {classification = a} :: CreateGrokClassifierRequest)
{-# DEPRECATED cgcrClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The name of the new classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrName :: Lens.Lens' CreateGrokClassifierRequest Lude.Text
cgcrName = Lens.lens (name :: CreateGrokClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: CreateGrokClassifierRequest)
{-# DEPRECATED cgcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional custom grok patterns used by this classifier.
--
-- /Note:/ Consider using 'customPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrCustomPatterns :: Lens.Lens' CreateGrokClassifierRequest (Lude.Maybe Lude.Text)
cgcrCustomPatterns = Lens.lens (customPatterns :: CreateGrokClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {customPatterns = a} :: CreateGrokClassifierRequest)
{-# DEPRECATED cgcrCustomPatterns "Use generic-lens or generic-optics with 'customPatterns' instead." #-}

-- | The grok pattern used by this classifier.
--
-- /Note:/ Consider using 'grokPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrGrokPattern :: Lens.Lens' CreateGrokClassifierRequest Lude.Text
cgcrGrokPattern = Lens.lens (grokPattern :: CreateGrokClassifierRequest -> Lude.Text) (\s a -> s {grokPattern = a} :: CreateGrokClassifierRequest)
{-# DEPRECATED cgcrGrokPattern "Use generic-lens or generic-optics with 'grokPattern' instead." #-}

instance Lude.ToJSON CreateGrokClassifierRequest where
  toJSON CreateGrokClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Classification" Lude..= classification),
            Lude.Just ("Name" Lude..= name),
            ("CustomPatterns" Lude..=) Lude.<$> customPatterns,
            Lude.Just ("GrokPattern" Lude..= grokPattern)
          ]
      )
