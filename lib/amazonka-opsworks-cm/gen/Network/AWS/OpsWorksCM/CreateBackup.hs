{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.CreateBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application-level backup of a server. While the server is in the @BACKING_UP@ state, the server cannot be changed, and no additional backup can be created.
--
-- Backups can be created for servers in @RUNNING@ , @HEALTHY@ , and @UNHEALTHY@ states. By default, you can create a maximum of 50 manual backups.
-- This operation is asynchronous.
-- A @LimitExceededException@ is thrown when the maximum number of manual backups is reached. An @InvalidStateException@ is thrown when the server is not in any of the following states: RUNNING, HEALTHY, or UNHEALTHY. A @ResourceNotFoundException@ is thrown when the server is not found. A @ValidationException@ is thrown when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.CreateBackup
  ( -- * Creating a request
    CreateBackup (..),
    mkCreateBackup,

    -- ** Request lenses
    cbServerName,
    cbDescription,
    cbTags,

    -- * Destructuring the response
    CreateBackupResponse (..),
    mkCreateBackupResponse,

    -- ** Response lenses
    cbrrsBackup,
    cbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { -- | The name of the server that you want to back up.
    serverName :: Types.ServerName,
    -- | A user-defined description of the backup.
    description :: Core.Maybe Types.String,
    -- | A map that contains tag keys and tag values to attach to an AWS OpsWorks-CM server backup.
    --
    --
    --     * The key cannot be empty.
    --
    --
    --     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    --
    --
    --     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
    --
    --
    --     * Leading and trailing white spaces are trimmed from both the key and value.
    --
    --
    --     * A maximum of 50 user-applied tags is allowed for tag-supported AWS OpsWorks-CM resources.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBackup' value with any optional fields omitted.
mkCreateBackup ::
  -- | 'serverName'
  Types.ServerName ->
  CreateBackup
mkCreateBackup serverName =
  CreateBackup'
    { serverName,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the server that you want to back up.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbServerName :: Lens.Lens' CreateBackup Types.ServerName
cbServerName = Lens.field @"serverName"
{-# DEPRECATED cbServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | A user-defined description of the backup.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbDescription :: Lens.Lens' CreateBackup (Core.Maybe Types.String)
cbDescription = Lens.field @"description"
{-# DEPRECATED cbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A map that contains tag keys and tag values to attach to an AWS OpsWorks-CM server backup.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 50 user-applied tags is allowed for tag-supported AWS OpsWorks-CM resources.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTags :: Lens.Lens' CreateBackup (Core.Maybe [Types.Tag])
cbTags = Lens.field @"tags"
{-# DEPRECATED cbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateBackup where
  toJSON CreateBackup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServerName" Core..= serverName),
            ("Description" Core..=) Core.<$> description,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateBackup where
  type Rs CreateBackup = CreateBackupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.CreateBackup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupResponse'
            Core.<$> (x Core..:? "Backup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { -- | Backup created by request.
    backup :: Core.Maybe Types.Backup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateBackupResponse' value with any optional fields omitted.
mkCreateBackupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateBackupResponse
mkCreateBackupResponse responseStatus =
  CreateBackupResponse' {backup = Core.Nothing, responseStatus}

-- | Backup created by request.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsBackup :: Lens.Lens' CreateBackupResponse (Core.Maybe Types.Backup)
cbrrsBackup = Lens.field @"backup"
{-# DEPRECATED cbrrsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsResponseStatus :: Lens.Lens' CreateBackupResponse Core.Int
cbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
