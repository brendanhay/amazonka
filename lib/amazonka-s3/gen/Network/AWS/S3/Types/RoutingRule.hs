-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RoutingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RoutingRule
  ( RoutingRule (..),

    -- * Smart constructor
    mkRoutingRule,

    -- * Lenses
    rrCondition,
    rrRedirect,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Condition
import Network.AWS.S3.Types.Redirect

-- | Specifies the redirect behavior and when a redirect is applied. For more information about routing rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html#advanced-conditional-redirects Configuring advanced conditional redirects> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkRoutingRule' smart constructor.
data RoutingRule = RoutingRule'
  { condition :: Lude.Maybe Condition,
    redirect :: Redirect
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingRule' with the minimum fields required to make a request.
--
-- * 'condition' - A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
-- * 'redirect' - Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
mkRoutingRule ::
  -- | 'redirect'
  Redirect ->
  RoutingRule
mkRoutingRule pRedirect_ =
  RoutingRule' {condition = Lude.Nothing, redirect = pRedirect_}

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCondition :: Lens.Lens' RoutingRule (Lude.Maybe Condition)
rrCondition = Lens.lens (condition :: RoutingRule -> Lude.Maybe Condition) (\s a -> s {condition = a} :: RoutingRule)
{-# DEPRECATED rrCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

-- | Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
--
-- /Note:/ Consider using 'redirect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRedirect :: Lens.Lens' RoutingRule Redirect
rrRedirect = Lens.lens (redirect :: RoutingRule -> Redirect) (\s a -> s {redirect = a} :: RoutingRule)
{-# DEPRECATED rrRedirect "Use generic-lens or generic-optics with 'redirect' instead." #-}

instance Lude.FromXML RoutingRule where
  parseXML x =
    RoutingRule'
      Lude.<$> (x Lude..@? "Condition") Lude.<*> (x Lude..@ "Redirect")

instance Lude.ToXML RoutingRule where
  toXML RoutingRule' {..} =
    Lude.mconcat
      ["Condition" Lude.@= condition, "Redirect" Lude.@= redirect]
