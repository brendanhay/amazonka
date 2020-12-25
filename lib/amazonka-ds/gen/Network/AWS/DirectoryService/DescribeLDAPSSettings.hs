{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeLDAPSSettings (..),
    mkDescribeLDAPSSettings,

    -- ** Request lenses
    dldapssDirectoryId,
    dldapssLimit,
    dldapssNextToken,
    dldapssType,

    -- * Destructuring the response
    DescribeLDAPSSettingsResponse (..),
    mkDescribeLDAPSSettingsResponse,

    -- ** Response lenses
    dldapssrrsLDAPSSettingsInfo,
    dldapssrrsNextToken,
    dldapssrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLDAPSSettings' smart constructor.
data DescribeLDAPSSettings = DescribeLDAPSSettings'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | Specifies the number of items that should be displayed on one page.
    limit :: Core.Maybe Core.Natural,
    -- | The type of next token used for pagination.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
    type' :: Core.Maybe Types.LDAPSType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLDAPSSettings' value with any optional fields omitted.
mkDescribeLDAPSSettings ::
  -- | 'directoryId'
  Types.DirectoryId ->
  DescribeLDAPSSettings
mkDescribeLDAPSSettings directoryId =
  DescribeLDAPSSettings'
    { directoryId,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      type' = Core.Nothing
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssDirectoryId :: Lens.Lens' DescribeLDAPSSettings Types.DirectoryId
dldapssDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dldapssDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | Specifies the number of items that should be displayed on one page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssLimit :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Core.Natural)
dldapssLimit = Lens.field @"limit"
{-# DEPRECATED dldapssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The type of next token used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssNextToken :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Types.NextToken)
dldapssNextToken = Lens.field @"nextToken"
{-# DEPRECATED dldapssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssType :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Types.LDAPSType)
dldapssType = Lens.field @"type'"
{-# DEPRECATED dldapssType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DescribeLDAPSSettings where
  toJSON DescribeLDAPSSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.AWSRequest DescribeLDAPSSettings where
  type Rs DescribeLDAPSSettings = DescribeLDAPSSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DescribeLDAPSSettings")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLDAPSSettingsResponse'
            Core.<$> (x Core..:? "LDAPSSettingsInfo")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLDAPSSettingsResponse' smart constructor.
data DescribeLDAPSSettingsResponse = DescribeLDAPSSettingsResponse'
  { -- | Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
    lDAPSSettingsInfo :: Core.Maybe [Types.LDAPSSettingInfo],
    -- | The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeLDAPSSettingsResponse' value with any optional fields omitted.
mkDescribeLDAPSSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLDAPSSettingsResponse
mkDescribeLDAPSSettingsResponse responseStatus =
  DescribeLDAPSSettingsResponse'
    { lDAPSSettingsInfo = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
--
-- /Note:/ Consider using 'lDAPSSettingsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrrsLDAPSSettingsInfo :: Lens.Lens' DescribeLDAPSSettingsResponse (Core.Maybe [Types.LDAPSSettingInfo])
dldapssrrsLDAPSSettingsInfo = Lens.field @"lDAPSSettingsInfo"
{-# DEPRECATED dldapssrrsLDAPSSettingsInfo "Use generic-lens or generic-optics with 'lDAPSSettingsInfo' instead." #-}

-- | The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrrsNextToken :: Lens.Lens' DescribeLDAPSSettingsResponse (Core.Maybe Types.NextToken)
dldapssrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dldapssrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrrsResponseStatus :: Lens.Lens' DescribeLDAPSSettingsResponse Core.Int
dldapssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dldapssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
