{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.WebsiteConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.WebsiteConfiguration
  ( WebsiteConfiguration (..),

    -- * Smart constructor
    mkWebsiteConfiguration,

    -- * Lenses
    wcRedirectAllRequestsTo,
    wcErrorDocument,
    wcIndexDocument,
    wcRoutingRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ErrorDocument
import Network.AWS.S3.Types.IndexDocument
import Network.AWS.S3.Types.RedirectAllRequestsTo
import Network.AWS.S3.Types.RoutingRule

-- | Specifies website configuration parameters for an Amazon S3 bucket.
--
-- /See:/ 'mkWebsiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
  { redirectAllRequestsTo ::
      Lude.Maybe RedirectAllRequestsTo,
    errorDocument :: Lude.Maybe ErrorDocument,
    indexDocument :: Lude.Maybe IndexDocument,
    routingRules :: Lude.Maybe [RoutingRule]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebsiteConfiguration' with the minimum fields required to make a request.
--
-- * 'errorDocument' - The name of the error document for the website.
-- * 'indexDocument' - The name of the index document for the website.
-- * 'redirectAllRequestsTo' - The redirect behavior for every request to this bucket's website endpoint.
--
-- /Important:/ If you specify this property, you can't specify any other property.
-- * 'routingRules' - Rules that define when a redirect is applied and the redirect behavior.
mkWebsiteConfiguration ::
  WebsiteConfiguration
mkWebsiteConfiguration =
  WebsiteConfiguration'
    { redirectAllRequestsTo = Lude.Nothing,
      errorDocument = Lude.Nothing,
      indexDocument = Lude.Nothing,
      routingRules = Lude.Nothing
    }

-- | The redirect behavior for every request to this bucket's website endpoint.
--
-- /Important:/ If you specify this property, you can't specify any other property.
--
-- /Note:/ Consider using 'redirectAllRequestsTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcRedirectAllRequestsTo :: Lens.Lens' WebsiteConfiguration (Lude.Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = Lens.lens (redirectAllRequestsTo :: WebsiteConfiguration -> Lude.Maybe RedirectAllRequestsTo) (\s a -> s {redirectAllRequestsTo = a} :: WebsiteConfiguration)
{-# DEPRECATED wcRedirectAllRequestsTo "Use generic-lens or generic-optics with 'redirectAllRequestsTo' instead." #-}

-- | The name of the error document for the website.
--
-- /Note:/ Consider using 'errorDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcErrorDocument :: Lens.Lens' WebsiteConfiguration (Lude.Maybe ErrorDocument)
wcErrorDocument = Lens.lens (errorDocument :: WebsiteConfiguration -> Lude.Maybe ErrorDocument) (\s a -> s {errorDocument = a} :: WebsiteConfiguration)
{-# DEPRECATED wcErrorDocument "Use generic-lens or generic-optics with 'errorDocument' instead." #-}

-- | The name of the index document for the website.
--
-- /Note:/ Consider using 'indexDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcIndexDocument :: Lens.Lens' WebsiteConfiguration (Lude.Maybe IndexDocument)
wcIndexDocument = Lens.lens (indexDocument :: WebsiteConfiguration -> Lude.Maybe IndexDocument) (\s a -> s {indexDocument = a} :: WebsiteConfiguration)
{-# DEPRECATED wcIndexDocument "Use generic-lens or generic-optics with 'indexDocument' instead." #-}

-- | Rules that define when a redirect is applied and the redirect behavior.
--
-- /Note:/ Consider using 'routingRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcRoutingRules :: Lens.Lens' WebsiteConfiguration (Lude.Maybe [RoutingRule])
wcRoutingRules = Lens.lens (routingRules :: WebsiteConfiguration -> Lude.Maybe [RoutingRule]) (\s a -> s {routingRules = a} :: WebsiteConfiguration)
{-# DEPRECATED wcRoutingRules "Use generic-lens or generic-optics with 'routingRules' instead." #-}

instance Lude.ToXML WebsiteConfiguration where
  toXML WebsiteConfiguration' {..} =
    Lude.mconcat
      [ "RedirectAllRequestsTo" Lude.@= redirectAllRequestsTo,
        "ErrorDocument" Lude.@= errorDocument,
        "IndexDocument" Lude.@= indexDocument,
        "RoutingRules"
          Lude.@= Lude.toXML (Lude.toXMLList "RoutingRule" Lude.<$> routingRules)
      ]
