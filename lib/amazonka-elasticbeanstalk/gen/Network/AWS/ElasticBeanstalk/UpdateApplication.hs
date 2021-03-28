{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application to have the specified properties.
module Network.AWS.ElasticBeanstalk.UpdateApplication
    (
    -- * Creating a request
      UpdateApplication (..)
    , mkUpdateApplication
    -- ** Request lenses
    , uaApplicationName
    , uaDescription

     -- * Destructuring the response
    , Types.ApplicationDescriptionMessage (..)
    , Types.mkApplicationDescriptionMessage
    -- ** Response lenses
    , Types.admApplication
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an application.
--
-- /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { applicationName :: Types.ApplicationName
    -- ^ The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error. 
  , description :: Core.Maybe Types.Description
    -- ^ A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplication' value with any optional fields omitted.
mkUpdateApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> UpdateApplication
mkUpdateApplication applicationName
  = UpdateApplication'{applicationName, description = Core.Nothing}

-- | The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error. 
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication Types.ApplicationName
uaApplicationName = Lens.field @"applicationName"
{-# INLINEABLE uaApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApplication (Core.Maybe Types.Description)
uaDescription = Lens.field @"description"
{-# INLINEABLE uaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateApplication where
        toQuery UpdateApplication{..}
          = Core.toQueryPair "Action" ("UpdateApplication" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ApplicationName" applicationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description

instance Core.ToHeaders UpdateApplication where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateApplication where
        type Rs UpdateApplication = Types.ApplicationDescriptionMessage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "UpdateApplicationResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
