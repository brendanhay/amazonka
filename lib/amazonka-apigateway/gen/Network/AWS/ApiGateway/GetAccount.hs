{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'Account' resource.
module Network.AWS.ApiGateway.GetAccount
    (
    -- * Creating a request
      GetAccount (..)
    , mkGetAccount

     -- * Destructuring the response
    , Types.Account (..)
    , Types.mkAccount
    -- ** Response lenses
    , Types.aApiKeyVersion
    , Types.aCloudwatchRoleArn
    , Types.aFeatures
    , Types.aThrottleSettings
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about the current 'Account' resource.
--
-- /See:/ 'mkGetAccount' smart constructor.
data GetAccount = GetAccount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccount' value with any optional fields omitted.
mkGetAccount
    :: GetAccount
mkGetAccount = GetAccount'

instance Core.ToQuery GetAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAccount where
        toHeaders GetAccount{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetAccount where
        type Rs GetAccount = Types.Account
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/account",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
