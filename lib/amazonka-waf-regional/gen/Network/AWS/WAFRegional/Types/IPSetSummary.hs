{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.IPSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.IPSetSummary
  ( IPSetSummary (..),

    -- * Smart constructor
    mkIPSetSummary,

    -- * Lenses
    issName,
    issIPSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the identifier and the name of the @IPSet@ .
--
-- /See:/ 'mkIPSetSummary' smart constructor.
data IPSetSummary = IPSetSummary'
  { -- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
    name :: Lude.Text,
    -- | The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
-- * 'ipSetId' - The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
mkIPSetSummary ::
  -- | 'name'
  Lude.Text ->
  -- | 'ipSetId'
  Lude.Text ->
  IPSetSummary
mkIPSetSummary pName_ pIPSetId_ =
  IPSetSummary' {name = pName_, ipSetId = pIPSetId_}

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issName :: Lens.Lens' IPSetSummary Lude.Text
issName = Lens.lens (name :: IPSetSummary -> Lude.Text) (\s a -> s {name = a} :: IPSetSummary)
{-# DEPRECATED issName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issIPSetId :: Lens.Lens' IPSetSummary Lude.Text
issIPSetId = Lens.lens (ipSetId :: IPSetSummary -> Lude.Text) (\s a -> s {ipSetId = a} :: IPSetSummary)
{-# DEPRECATED issIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.FromJSON IPSetSummary where
  parseJSON =
    Lude.withObject
      "IPSetSummary"
      ( \x ->
          IPSetSummary'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "IPSetId")
      )
