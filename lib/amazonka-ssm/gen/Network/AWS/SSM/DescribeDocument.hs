{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Systems Manager document.
module Network.AWS.SSM.DescribeDocument
  ( -- * Creating a request
    DescribeDocument (..),
    mkDescribeDocument,

    -- ** Request lenses
    dName,
    dDocumentVersion,
    dVersionName,

    -- * Destructuring the response
    DescribeDocumentResponse (..),
    mkDescribeDocumentResponse,

    -- ** Response lenses
    ddrrsDocument,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeDocument' smart constructor.
data DescribeDocument = DescribeDocument'
  { -- | The name of the Systems Manager document.
    name :: Types.DocumentARN,
    -- | The document version for which you want information. Can be a specific version or the default version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
    versionName :: Core.Maybe Types.DocumentVersionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDocument' value with any optional fields omitted.
mkDescribeDocument ::
  -- | 'name'
  Types.DocumentARN ->
  DescribeDocument
mkDescribeDocument name =
  DescribeDocument'
    { name,
      documentVersion = Core.Nothing,
      versionName = Core.Nothing
    }

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DescribeDocument Types.DocumentARN
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The document version for which you want information. Can be a specific version or the default version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDocumentVersion :: Lens.Lens' DescribeDocument (Core.Maybe Types.DocumentVersion)
dDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED dDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionName :: Lens.Lens' DescribeDocument (Core.Maybe Types.DocumentVersionName)
dVersionName = Lens.field @"versionName"
{-# DEPRECATED dVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

instance Core.FromJSON DescribeDocument where
  toJSON DescribeDocument {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("VersionName" Core..=) Core.<$> versionName
          ]
      )

instance Core.AWSRequest DescribeDocument where
  type Rs DescribeDocument = DescribeDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeDocument")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentResponse'
            Core.<$> (x Core..:? "Document") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDocumentResponse' smart constructor.
data DescribeDocumentResponse = DescribeDocumentResponse'
  { -- | Information about the Systems Manager document.
    document :: Core.Maybe Types.DocumentDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDocumentResponse' value with any optional fields omitted.
mkDescribeDocumentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDocumentResponse
mkDescribeDocumentResponse responseStatus =
  DescribeDocumentResponse'
    { document = Core.Nothing,
      responseStatus
    }

-- | Information about the Systems Manager document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDocument :: Lens.Lens' DescribeDocumentResponse (Core.Maybe Types.DocumentDescription)
ddrrsDocument = Lens.field @"document"
{-# DEPRECATED ddrrsDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDocumentResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
