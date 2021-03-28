{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeLDAPSSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of LDAP security for the specified directory.
module Network.AWS.DirectoryService.DescribeLDAPSSettings
    (
    -- * Creating a request
      DescribeLDAPSSettings (..)
    , mkDescribeLDAPSSettings
    -- ** Request lenses
    , dldapssDirectoryId
    , dldapssLimit
    , dldapssNextToken
    , dldapssType

    -- * Destructuring the response
    , DescribeLDAPSSettingsResponse (..)
    , mkDescribeLDAPSSettingsResponse
    -- ** Response lenses
    , dldapssrrsLDAPSSettingsInfo
    , dldapssrrsNextToken
    , dldapssrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLDAPSSettings' smart constructor.
data DescribeLDAPSSettings = DescribeLDAPSSettings'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , limit :: Core.Maybe Core.Natural
    -- ^ Specifies the number of items that should be displayed on one page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The type of next token used for pagination.
  , type' :: Core.Maybe Types.LDAPSType
    -- ^ The type of LDAP security to enable. Currently only the value @Client@ is supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLDAPSSettings' value with any optional fields omitted.
mkDescribeLDAPSSettings
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DescribeLDAPSSettings
mkDescribeLDAPSSettings directoryId
  = DescribeLDAPSSettings'{directoryId, limit = Core.Nothing,
                           nextToken = Core.Nothing, type' = Core.Nothing}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssDirectoryId :: Lens.Lens' DescribeLDAPSSettings Types.DirectoryId
dldapssDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dldapssDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | Specifies the number of items that should be displayed on one page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssLimit :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Core.Natural)
dldapssLimit = Lens.field @"limit"
{-# INLINEABLE dldapssLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The type of next token used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssNextToken :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Types.NextToken)
dldapssNextToken = Lens.field @"nextToken"
{-# INLINEABLE dldapssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssType :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Types.LDAPSType)
dldapssType = Lens.field @"type'"
{-# INLINEABLE dldapssType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery DescribeLDAPSSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeLDAPSSettings where
        toHeaders DescribeLDAPSSettings{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DescribeLDAPSSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeLDAPSSettings where
        toJSON DescribeLDAPSSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Type" Core..=) Core.<$> type'])

instance Core.AWSRequest DescribeLDAPSSettings where
        type Rs DescribeLDAPSSettings = DescribeLDAPSSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeLDAPSSettingsResponse' Core.<$>
                   (x Core..:? "LDAPSSettingsInfo") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeLDAPSSettingsResponse' smart constructor.
data DescribeLDAPSSettingsResponse = DescribeLDAPSSettingsResponse'
  { lDAPSSettingsInfo :: Core.Maybe [Types.LDAPSSettingInfo]
    -- ^ Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeLDAPSSettingsResponse' value with any optional fields omitted.
mkDescribeLDAPSSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLDAPSSettingsResponse
mkDescribeLDAPSSettingsResponse responseStatus
  = DescribeLDAPSSettingsResponse'{lDAPSSettingsInfo = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
--
-- /Note:/ Consider using 'lDAPSSettingsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrrsLDAPSSettingsInfo :: Lens.Lens' DescribeLDAPSSettingsResponse (Core.Maybe [Types.LDAPSSettingInfo])
dldapssrrsLDAPSSettingsInfo = Lens.field @"lDAPSSettingsInfo"
{-# INLINEABLE dldapssrrsLDAPSSettingsInfo #-}
{-# DEPRECATED lDAPSSettingsInfo "Use generic-lens or generic-optics with 'lDAPSSettingsInfo' instead"  #-}

-- | The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrrsNextToken :: Lens.Lens' DescribeLDAPSSettingsResponse (Core.Maybe Types.NextToken)
dldapssrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dldapssrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrrsResponseStatus :: Lens.Lens' DescribeLDAPSSettingsResponse Core.Int
dldapssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dldapssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
