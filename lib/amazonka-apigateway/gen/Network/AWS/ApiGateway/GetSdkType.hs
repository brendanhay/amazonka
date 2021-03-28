{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetSdkType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.GetSdkType
    (
    -- * Creating a request
      GetSdkType (..)
    , mkGetSdkType
    -- ** Request lenses
    , gstId

     -- * Destructuring the response
    , Types.SdkType (..)
    , Types.mkSdkType
    -- ** Response lenses
    , Types.stConfigurationProperties
    , Types.stDescription
    , Types.stFriendlyName
    , Types.stId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Get an 'SdkType' instance.
--
-- /See:/ 'mkGetSdkType' smart constructor.
newtype GetSdkType = GetSdkType'
  { id :: Core.Text
    -- ^ [Required] The identifier of the queried 'SdkType' instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSdkType' value with any optional fields omitted.
mkGetSdkType
    :: Core.Text -- ^ 'id'
    -> GetSdkType
mkGetSdkType id = GetSdkType'{id}

-- | [Required] The identifier of the queried 'SdkType' instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstId :: Lens.Lens' GetSdkType Core.Text
gstId = Lens.field @"id"
{-# INLINEABLE gstId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetSdkType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSdkType where
        toHeaders GetSdkType{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetSdkType where
        type Rs GetSdkType = Types.SdkType
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/sdktypes/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
