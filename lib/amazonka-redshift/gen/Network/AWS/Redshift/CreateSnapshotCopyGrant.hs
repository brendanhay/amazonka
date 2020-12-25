{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotCopyGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot copy grant that permits Amazon Redshift to use a customer master key (CMK) from AWS Key Management Service (AWS KMS) to encrypt copied snapshots in a destination region.
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateSnapshotCopyGrant
  ( -- * Creating a request
    CreateSnapshotCopyGrant (..),
    mkCreateSnapshotCopyGrant,

    -- ** Request lenses
    cscgSnapshotCopyGrantName,
    cscgKmsKeyId,
    cscgTags,

    -- * Destructuring the response
    CreateSnapshotCopyGrantResponse (..),
    mkCreateSnapshotCopyGrantResponse,

    -- ** Response lenses
    cscgrrsSnapshotCopyGrant,
    cscgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result of the @CreateSnapshotCopyGrant@ action.
--
-- /See:/ 'mkCreateSnapshotCopyGrant' smart constructor.
data CreateSnapshotCopyGrant = CreateSnapshotCopyGrant'
  { -- | The name of the snapshot copy grant. This name must be unique in the region for the AWS account.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    --
    --     * Alphabetic characters must be lowercase.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    --
    --
    --     * Must be unique for all clusters within an AWS account.
    snapshotCopyGrantName :: Types.String,
    -- | The unique identifier of the customer master key (CMK) to which to grant Amazon Redshift permission. If no key is specified, the default key is used.
    kmsKeyId :: Core.Maybe Types.String,
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotCopyGrant' value with any optional fields omitted.
mkCreateSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Types.String ->
  CreateSnapshotCopyGrant
mkCreateSnapshotCopyGrant snapshotCopyGrantName =
  CreateSnapshotCopyGrant'
    { snapshotCopyGrantName,
      kmsKeyId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the snapshot copy grant. This name must be unique in the region for the AWS account.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * Alphabetic characters must be lowercase.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for all clusters within an AWS account.
--
--
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgSnapshotCopyGrantName :: Lens.Lens' CreateSnapshotCopyGrant Types.String
cscgSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# DEPRECATED cscgSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

-- | The unique identifier of the customer master key (CMK) to which to grant Amazon Redshift permission. If no key is specified, the default key is used.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgKmsKeyId :: Lens.Lens' CreateSnapshotCopyGrant (Core.Maybe Types.String)
cscgKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cscgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgTags :: Lens.Lens' CreateSnapshotCopyGrant (Core.Maybe [Types.Tag])
cscgTags = Lens.field @"tags"
{-# DEPRECATED cscgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateSnapshotCopyGrant where
  type Rs CreateSnapshotCopyGrant = CreateSnapshotCopyGrantResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateSnapshotCopyGrant")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SnapshotCopyGrantName" snapshotCopyGrantName)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotCopyGrantResult"
      ( \s h x ->
          CreateSnapshotCopyGrantResponse'
            Core.<$> (x Core..@? "SnapshotCopyGrant")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSnapshotCopyGrantResponse' smart constructor.
data CreateSnapshotCopyGrantResponse = CreateSnapshotCopyGrantResponse'
  { snapshotCopyGrant :: Core.Maybe Types.SnapshotCopyGrant,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshotCopyGrantResponse' value with any optional fields omitted.
mkCreateSnapshotCopyGrantResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSnapshotCopyGrantResponse
mkCreateSnapshotCopyGrantResponse responseStatus =
  CreateSnapshotCopyGrantResponse'
    { snapshotCopyGrant =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshotCopyGrant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgrrsSnapshotCopyGrant :: Lens.Lens' CreateSnapshotCopyGrantResponse (Core.Maybe Types.SnapshotCopyGrant)
cscgrrsSnapshotCopyGrant = Lens.field @"snapshotCopyGrant"
{-# DEPRECATED cscgrrsSnapshotCopyGrant "Use generic-lens or generic-optics with 'snapshotCopyGrant' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgrrsResponseStatus :: Lens.Lens' CreateSnapshotCopyGrantResponse Core.Int
cscgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cscgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
