{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RoutingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.RoutingRule
  ( RoutingRule (..)
  -- * Smart constructor
  , mkRoutingRule
  -- * Lenses
  , rrRedirect
  , rrCondition
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Condition as Types
import qualified Network.AWS.S3.Types.Redirect as Types

-- | Specifies the redirect behavior and when a redirect is applied. For more information about routing rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html#advanced-conditional-redirects Configuring advanced conditional redirects> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkRoutingRule' smart constructor.
data RoutingRule = RoutingRule'
  { redirect :: Types.Redirect
    -- ^ Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
  , condition :: Core.Maybe Types.Condition
    -- ^ A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingRule' value with any optional fields omitted.
mkRoutingRule
    :: Types.Redirect -- ^ 'redirect'
    -> RoutingRule
mkRoutingRule redirect
  = RoutingRule'{redirect, condition = Core.Nothing}

-- | Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can specify a different error code to return.
--
-- /Note:/ Consider using 'redirect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrRedirect :: Lens.Lens' RoutingRule Types.Redirect
rrRedirect = Lens.field @"redirect"
{-# INLINEABLE rrRedirect #-}
{-# DEPRECATED redirect "Use generic-lens or generic-optics with 'redirect' instead"  #-}

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCondition :: Lens.Lens' RoutingRule (Core.Maybe Types.Condition)
rrCondition = Lens.field @"condition"
{-# INLINEABLE rrCondition #-}
{-# DEPRECATED condition "Use generic-lens or generic-optics with 'condition' instead"  #-}

instance Core.ToXML RoutingRule where
        toXML RoutingRule{..}
          = Core.toXMLElement "Redirect" redirect Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Condition") condition

instance Core.FromXML RoutingRule where
        parseXML x
          = RoutingRule' Core.<$>
              (x Core..@ "Redirect") Core.<*> x Core..@? "Condition"
