-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WebACLSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WebACLSummary
  ( WebACLSummary (..),

    -- * Smart constructor
    mkWebACLSummary,

    -- * Lenses
    wasWebACLId,
    wasName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the identifier and the name or description of the 'WebACL' .
--
-- /See:/ 'mkWebACLSummary' smart constructor.
data WebACLSummary = WebACLSummary'
  { webACLId :: Lude.Text,
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

-- | Creates a value of 'WebACLSummary' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
-- * 'webACLId' - A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
--
-- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
mkWebACLSummary ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  WebACLSummary
mkWebACLSummary pWebACLId_ pName_ =
  WebACLSummary' {webACLId = pWebACLId_, name = pName_}

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
--
-- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasWebACLId :: Lens.Lens' WebACLSummary Lude.Text
wasWebACLId = Lens.lens (webACLId :: WebACLSummary -> Lude.Text) (\s a -> s {webACLId = a} :: WebACLSummary)
{-# DEPRECATED wasWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasName :: Lens.Lens' WebACLSummary Lude.Text
wasName = Lens.lens (name :: WebACLSummary -> Lude.Text) (\s a -> s {name = a} :: WebACLSummary)
{-# DEPRECATED wasName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON WebACLSummary where
  parseJSON =
    Lude.withObject
      "WebACLSummary"
      ( \x ->
          WebACLSummary'
            Lude.<$> (x Lude..: "WebACLId") Lude.<*> (x Lude..: "Name")
      )
