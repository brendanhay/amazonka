{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpAction
  ( HttpAction (..)
  -- * Smart constructor
  , mkHttpAction
  -- * Lenses
  , haUrl
  , haAuth
  , haConfirmationUrl
  , haHeaders
  ) where

import qualified Network.AWS.IoT.Types.HttpActionHeader as Types
import qualified Network.AWS.IoT.Types.HttpAuthorization as Types
import qualified Network.AWS.IoT.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Send data to an HTTPS endpoint.
--
-- /See:/ 'mkHttpAction' smart constructor.
data HttpAction = HttpAction'
  { url :: Types.Url
    -- ^ The endpoint URL. If substitution templates are used in the URL, you must also specify a @confirmationUrl@ . If this is a new destination, a new @TopicRuleDestination@ is created if possible.
  , auth :: Core.Maybe Types.HttpAuthorization
    -- ^ The authentication method to use when sending data to an HTTPS endpoint.
  , confirmationUrl :: Core.Maybe Types.Url
    -- ^ The URL to which AWS IoT sends a confirmation message. The value of the confirmation URL must be a prefix of the endpoint URL. If you do not specify a confirmation URL AWS IoT uses the endpoint URL as the confirmation URL. If you use substitution templates in the confirmationUrl, you must create and enable topic rule destinations that match each possible value of the substitution template before traffic is allowed to your endpoint URL.
  , headers :: Core.Maybe [Types.HttpActionHeader]
    -- ^ The HTTP headers to send with the message data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpAction' value with any optional fields omitted.
mkHttpAction
    :: Types.Url -- ^ 'url'
    -> HttpAction
mkHttpAction url
  = HttpAction'{url, auth = Core.Nothing,
                confirmationUrl = Core.Nothing, headers = Core.Nothing}

-- | The endpoint URL. If substitution templates are used in the URL, you must also specify a @confirmationUrl@ . If this is a new destination, a new @TopicRuleDestination@ is created if possible.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haUrl :: Lens.Lens' HttpAction Types.Url
haUrl = Lens.field @"url"
{-# INLINEABLE haUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The authentication method to use when sending data to an HTTPS endpoint.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haAuth :: Lens.Lens' HttpAction (Core.Maybe Types.HttpAuthorization)
haAuth = Lens.field @"auth"
{-# INLINEABLE haAuth #-}
{-# DEPRECATED auth "Use generic-lens or generic-optics with 'auth' instead"  #-}

-- | The URL to which AWS IoT sends a confirmation message. The value of the confirmation URL must be a prefix of the endpoint URL. If you do not specify a confirmation URL AWS IoT uses the endpoint URL as the confirmation URL. If you use substitution templates in the confirmationUrl, you must create and enable topic rule destinations that match each possible value of the substitution template before traffic is allowed to your endpoint URL.
--
-- /Note:/ Consider using 'confirmationUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haConfirmationUrl :: Lens.Lens' HttpAction (Core.Maybe Types.Url)
haConfirmationUrl = Lens.field @"confirmationUrl"
{-# INLINEABLE haConfirmationUrl #-}
{-# DEPRECATED confirmationUrl "Use generic-lens or generic-optics with 'confirmationUrl' instead"  #-}

-- | The HTTP headers to send with the message data.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haHeaders :: Lens.Lens' HttpAction (Core.Maybe [Types.HttpActionHeader])
haHeaders = Lens.field @"headers"
{-# INLINEABLE haHeaders #-}
{-# DEPRECATED headers "Use generic-lens or generic-optics with 'headers' instead"  #-}

instance Core.FromJSON HttpAction where
        toJSON HttpAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("url" Core..= url), ("auth" Core..=) Core.<$> auth,
                  ("confirmationUrl" Core..=) Core.<$> confirmationUrl,
                  ("headers" Core..=) Core.<$> headers])

instance Core.FromJSON HttpAction where
        parseJSON
          = Core.withObject "HttpAction" Core.$
              \ x ->
                HttpAction' Core.<$>
                  (x Core..: "url") Core.<*> x Core..:? "auth" Core.<*>
                    x Core..:? "confirmationUrl"
                    Core.<*> x Core..:? "headers"
