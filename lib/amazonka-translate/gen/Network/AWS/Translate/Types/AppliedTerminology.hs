-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.AppliedTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.AppliedTerminology
  ( AppliedTerminology (..),

    -- * Smart constructor
    mkAppliedTerminology,

    -- * Lenses
    atTerms,
    atName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.Term

-- | The custom terminology applied to the input text by Amazon Translate for the translated text response. This is optional in the response and will only be present if you specified terminology input in the request. Currently, only one terminology can be applied per TranslateText request.
--
-- /See:/ 'mkAppliedTerminology' smart constructor.
data AppliedTerminology = AppliedTerminology'
  { terms ::
      Lude.Maybe [Term],
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

-- | Creates a value of 'AppliedTerminology' with the minimum fields required to make a request.
--
-- * 'name' - The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
-- * 'terms' - The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
mkAppliedTerminology ::
  AppliedTerminology
mkAppliedTerminology =
  AppliedTerminology' {terms = Lude.Nothing, name = Lude.Nothing}

-- | The specific terms of the custom terminology applied to the input text by Amazon Translate for the translated text response. A maximum of 250 terms will be returned, and the specific terms applied will be the first 250 terms in the source text.
--
-- /Note:/ Consider using 'terms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atTerms :: Lens.Lens' AppliedTerminology (Lude.Maybe [Term])
atTerms = Lens.lens (terms :: AppliedTerminology -> Lude.Maybe [Term]) (\s a -> s {terms = a} :: AppliedTerminology)
{-# DEPRECATED atTerms "Use generic-lens or generic-optics with 'terms' instead." #-}

-- | The name of the custom terminology applied to the input text by Amazon Translate for the translated text response.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' AppliedTerminology (Lude.Maybe Lude.Text)
atName = Lens.lens (name :: AppliedTerminology -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AppliedTerminology)
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AppliedTerminology where
  parseJSON =
    Lude.withObject
      "AppliedTerminology"
      ( \x ->
          AppliedTerminology'
            Lude.<$> (x Lude..:? "Terms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
      )
