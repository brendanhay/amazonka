{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Directory' by copying the published schema into the directory. A directory cannot be created without a schema.
--
-- You can also quickly create a directory using a managed schema, called the @QuickStartSchema@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_managed.html Managed Schema> in the /Amazon Cloud Directory Developer Guide/ .
module Network.AWS.CloudDirectory.CreateDirectory
  ( -- * Creating a request
    CreateDirectory (..),
    mkCreateDirectory,

    -- ** Request lenses
    cdName,
    cdSchemaArn,

    -- * Destructuring the response
    CreateDirectoryResponse (..),
    mkCreateDirectoryResponse,

    -- ** Response lenses
    cdrrsDirectoryArn,
    cdrrsName,
    cdrrsObjectIdentifier,
    cdrrsAppliedSchemaArn,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { -- | The name of the 'Directory' . Should be unique per account, per region.
    name :: Types.Name,
    -- | The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
    schemaArn :: Types.SchemaArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectory' value with any optional fields omitted.
mkCreateDirectory ::
  -- | 'name'
  Types.Name ->
  -- | 'schemaArn'
  Types.SchemaArn ->
  CreateDirectory
mkCreateDirectory name schemaArn =
  CreateDirectory' {name, schemaArn}

-- | The name of the 'Directory' . Should be unique per account, per region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CreateDirectory Types.Name
cdName = Lens.field @"name"
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSchemaArn :: Lens.Lens' CreateDirectory Types.SchemaArn
cdSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED cdSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

instance Core.FromJSON CreateDirectory where
  toJSON CreateDirectory {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest CreateDirectory where
  type Rs CreateDirectory = CreateDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/directory/create",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectoryResponse'
            Core.<$> (x Core..: "DirectoryArn")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "ObjectIdentifier")
            Core.<*> (x Core..: "AppliedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { -- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
    directoryArn :: Types.DirectoryArn,
    -- | The name of the 'Directory' .
    name :: Types.DirectoryName,
    -- | The root object node of the created directory.
    objectIdentifier :: Types.ObjectIdentifier,
    -- | The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
    appliedSchemaArn :: Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectoryResponse' value with any optional fields omitted.
mkCreateDirectoryResponse ::
  -- | 'directoryArn'
  Types.DirectoryArn ->
  -- | 'name'
  Types.DirectoryName ->
  -- | 'objectIdentifier'
  Types.ObjectIdentifier ->
  -- | 'appliedSchemaArn'
  Types.Arn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateDirectoryResponse
mkCreateDirectoryResponse
  directoryArn
  name
  objectIdentifier
  appliedSchemaArn
  responseStatus =
    CreateDirectoryResponse'
      { directoryArn,
        name,
        objectIdentifier,
        appliedSchemaArn,
        responseStatus
      }

-- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDirectoryArn :: Lens.Lens' CreateDirectoryResponse Types.DirectoryArn
cdrrsDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED cdrrsDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The name of the 'Directory' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsName :: Lens.Lens' CreateDirectoryResponse Types.DirectoryName
cdrrsName = Lens.field @"name"
{-# DEPRECATED cdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The root object node of the created directory.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsObjectIdentifier :: Lens.Lens' CreateDirectoryResponse Types.ObjectIdentifier
cdrrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# DEPRECATED cdrrsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
--
-- /Note:/ Consider using 'appliedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsAppliedSchemaArn :: Lens.Lens' CreateDirectoryResponse Types.Arn
cdrrsAppliedSchemaArn = Lens.field @"appliedSchemaArn"
{-# DEPRECATED cdrrsAppliedSchemaArn "Use generic-lens or generic-optics with 'appliedSchemaArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDirectoryResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
