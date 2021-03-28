{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDirectory (..)
    , mkCreateDirectory
    -- ** Request lenses
    , cdName
    , cdSchemaArn

    -- * Destructuring the response
    , CreateDirectoryResponse (..)
    , mkCreateDirectoryResponse
    -- ** Response lenses
    , cdrrsDirectoryArn
    , cdrrsName
    , cdrrsObjectIdentifier
    , cdrrsAppliedSchemaArn
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { name :: Types.Name
    -- ^ The name of the 'Directory' . Should be unique per account, per region.
  , schemaArn :: Types.SchemaArn
    -- ^ The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectory' value with any optional fields omitted.
mkCreateDirectory
    :: Types.Name -- ^ 'name'
    -> Types.SchemaArn -- ^ 'schemaArn'
    -> CreateDirectory
mkCreateDirectory name schemaArn
  = CreateDirectory'{name, schemaArn}

-- | The name of the 'Directory' . Should be unique per account, per region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CreateDirectory Types.Name
cdName = Lens.field @"name"
{-# INLINEABLE cdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSchemaArn :: Lens.Lens' CreateDirectory Types.SchemaArn
cdSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE cdSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

instance Core.ToQuery CreateDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDirectory where
        toHeaders CreateDirectory{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON CreateDirectory where
        toJSON CreateDirectory{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest CreateDirectory where
        type Rs CreateDirectory = CreateDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/directory/create",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDirectoryResponse' Core.<$>
                   (x Core..: "DirectoryArn") Core.<*> x Core..: "Name" Core.<*>
                     x Core..: "ObjectIdentifier"
                     Core.<*> x Core..: "AppliedSchemaArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { directoryArn :: Types.DirectoryArn
    -- ^ The ARN that is associated with the 'Directory' . For more information, see 'arns' .
  , name :: Types.DirectoryName
    -- ^ The name of the 'Directory' .
  , objectIdentifier :: Types.ObjectIdentifier
    -- ^ The root object node of the created directory.
  , appliedSchemaArn :: Types.Arn
    -- ^ The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectoryResponse' value with any optional fields omitted.
mkCreateDirectoryResponse
    :: Types.DirectoryArn -- ^ 'directoryArn'
    -> Types.DirectoryName -- ^ 'name'
    -> Types.ObjectIdentifier -- ^ 'objectIdentifier'
    -> Types.Arn -- ^ 'appliedSchemaArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateDirectoryResponse
mkCreateDirectoryResponse directoryArn name objectIdentifier
  appliedSchemaArn responseStatus
  = CreateDirectoryResponse'{directoryArn, name, objectIdentifier,
                             appliedSchemaArn, responseStatus}

-- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDirectoryArn :: Lens.Lens' CreateDirectoryResponse Types.DirectoryArn
cdrrsDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE cdrrsDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The name of the 'Directory' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsName :: Lens.Lens' CreateDirectoryResponse Types.DirectoryName
cdrrsName = Lens.field @"name"
{-# INLINEABLE cdrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The root object node of the created directory.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsObjectIdentifier :: Lens.Lens' CreateDirectoryResponse Types.ObjectIdentifier
cdrrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE cdrrsObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

-- | The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
--
-- /Note:/ Consider using 'appliedSchemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsAppliedSchemaArn :: Lens.Lens' CreateDirectoryResponse Types.Arn
cdrrsAppliedSchemaArn = Lens.field @"appliedSchemaArn"
{-# INLINEABLE cdrrsAppliedSchemaArn #-}
{-# DEPRECATED appliedSchemaArn "Use generic-lens or generic-optics with 'appliedSchemaArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDirectoryResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
