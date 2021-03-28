{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the installation medium for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
module Network.AWS.RDS.DeleteInstallationMedia
    (
    -- * Creating a request
      DeleteInstallationMedia (..)
    , mkDeleteInstallationMedia
    -- ** Request lenses
    , dInstallationMediaId

     -- * Destructuring the response
    , Types.InstallationMedia (..)
    , Types.mkInstallationMedia
    -- ** Response lenses
    , Types.imCustomAvailabilityZoneId
    , Types.imEngine
    , Types.imEngineInstallationMediaPath
    , Types.imEngineVersion
    , Types.imFailureCause
    , Types.imInstallationMediaId
    , Types.imOSInstallationMediaPath
    , Types.imStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstallationMedia' smart constructor.
newtype DeleteInstallationMedia = DeleteInstallationMedia'
  { installationMediaId :: Core.Text
    -- ^ The installation medium ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstallationMedia' value with any optional fields omitted.
mkDeleteInstallationMedia
    :: Core.Text -- ^ 'installationMediaId'
    -> DeleteInstallationMedia
mkDeleteInstallationMedia installationMediaId
  = DeleteInstallationMedia'{installationMediaId}

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInstallationMediaId :: Lens.Lens' DeleteInstallationMedia Core.Text
dInstallationMediaId = Lens.field @"installationMediaId"
{-# INLINEABLE dInstallationMediaId #-}
{-# DEPRECATED installationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead"  #-}

instance Core.ToQuery DeleteInstallationMedia where
        toQuery DeleteInstallationMedia{..}
          = Core.toQueryPair "Action"
              ("DeleteInstallationMedia" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "InstallationMediaId" installationMediaId

instance Core.ToHeaders DeleteInstallationMedia where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteInstallationMedia where
        type Rs DeleteInstallationMedia = Types.InstallationMedia
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
          = Response.receiveXMLWrapper "DeleteInstallationMediaResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
