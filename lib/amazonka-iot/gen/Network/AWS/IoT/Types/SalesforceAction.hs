{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SalesforceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.SalesforceAction
  ( SalesforceAction (..)
  -- * Smart constructor
  , mkSalesforceAction
  -- * Lenses
  , saToken
  , saUrl
  ) where

import qualified Network.AWS.IoT.Types.SalesforceEndpoint as Types
import qualified Network.AWS.IoT.Types.Token as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to write a message to a Salesforce IoT Cloud Input Stream.
--
-- /See:/ 'mkSalesforceAction' smart constructor.
data SalesforceAction = SalesforceAction'
  { token :: Types.Token
    -- ^ The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
  , url :: Types.SalesforceEndpoint
    -- ^ The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SalesforceAction' value with any optional fields omitted.
mkSalesforceAction
    :: Types.Token -- ^ 'token'
    -> Types.SalesforceEndpoint -- ^ 'url'
    -> SalesforceAction
mkSalesforceAction token url = SalesforceAction'{token, url}

-- | The token used to authenticate access to the Salesforce IoT Cloud Input Stream. The token is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saToken :: Lens.Lens' SalesforceAction Types.Token
saToken = Lens.field @"token"
{-# INLINEABLE saToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

-- | The URL exposed by the Salesforce IoT Cloud Input Stream. The URL is available from the Salesforce IoT Cloud platform after creation of the Input Stream.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saUrl :: Lens.Lens' SalesforceAction Types.SalesforceEndpoint
saUrl = Lens.field @"url"
{-# INLINEABLE saUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON SalesforceAction where
        toJSON SalesforceAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("token" Core..= token), Core.Just ("url" Core..= url)])

instance Core.FromJSON SalesforceAction where
        parseJSON
          = Core.withObject "SalesforceAction" Core.$
              \ x ->
                SalesforceAction' Core.<$>
                  (x Core..: "token") Core.<*> x Core..: "url"
