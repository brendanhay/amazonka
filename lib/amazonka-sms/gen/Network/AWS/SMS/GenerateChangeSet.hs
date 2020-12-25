{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GenerateChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a target change set for a currently launched stack and writes it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Network.AWS.SMS.GenerateChangeSet
  ( -- * Creating a request
    GenerateChangeSet (..),
    mkGenerateChangeSet,

    -- ** Request lenses
    gcsAppId,
    gcsChangesetFormat,

    -- * Destructuring the response
    GenerateChangeSetResponse (..),
    mkGenerateChangeSetResponse,

    -- ** Response lenses
    gcsrrsS3Location,
    gcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGenerateChangeSet' smart constructor.
data GenerateChangeSet = GenerateChangeSet'
  { -- | The ID of the application associated with the change set.
    appId :: Core.Maybe Types.AppId,
    -- | The format for the change set.
    changesetFormat :: Core.Maybe Types.OutputFormat
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateChangeSet' value with any optional fields omitted.
mkGenerateChangeSet ::
  GenerateChangeSet
mkGenerateChangeSet =
  GenerateChangeSet'
    { appId = Core.Nothing,
      changesetFormat = Core.Nothing
    }

-- | The ID of the application associated with the change set.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsAppId :: Lens.Lens' GenerateChangeSet (Core.Maybe Types.AppId)
gcsAppId = Lens.field @"appId"
{-# DEPRECATED gcsAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The format for the change set.
--
-- /Note:/ Consider using 'changesetFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsChangesetFormat :: Lens.Lens' GenerateChangeSet (Core.Maybe Types.OutputFormat)
gcsChangesetFormat = Lens.field @"changesetFormat"
{-# DEPRECATED gcsChangesetFormat "Use generic-lens or generic-optics with 'changesetFormat' instead." #-}

instance Core.FromJSON GenerateChangeSet where
  toJSON GenerateChangeSet {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("changesetFormat" Core..=) Core.<$> changesetFormat
          ]
      )

instance Core.AWSRequest GenerateChangeSet where
  type Rs GenerateChangeSet = GenerateChangeSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.GenerateChangeSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateChangeSetResponse'
            Core.<$> (x Core..:? "s3Location") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGenerateChangeSetResponse' smart constructor.
data GenerateChangeSetResponse = GenerateChangeSetResponse'
  { -- | The location of the Amazon S3 object.
    s3Location :: Core.Maybe Types.S3Location,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenerateChangeSetResponse' value with any optional fields omitted.
mkGenerateChangeSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GenerateChangeSetResponse
mkGenerateChangeSetResponse responseStatus =
  GenerateChangeSetResponse'
    { s3Location = Core.Nothing,
      responseStatus
    }

-- | The location of the Amazon S3 object.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsS3Location :: Lens.Lens' GenerateChangeSetResponse (Core.Maybe Types.S3Location)
gcsrrsS3Location = Lens.field @"s3Location"
{-# DEPRECATED gcsrrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsResponseStatus :: Lens.Lens' GenerateChangeSetResponse Core.Int
gcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
