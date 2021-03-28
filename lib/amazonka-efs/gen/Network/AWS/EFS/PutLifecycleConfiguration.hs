{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables lifecycle management by creating a new @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object defines when files in an Amazon EFS file system are automatically transitioned to the lower-cost EFS Infrequent Access (IA) storage class. A @LifecycleConfiguration@ applies to all files in a file system.
--
-- Each Amazon EFS file system supports one lifecycle configuration, which applies to all files in the file system. If a @LifecycleConfiguration@ object already exists for the specified file system, a @PutLifecycleConfiguration@ call modifies the existing configuration. A @PutLifecycleConfiguration@ call with an empty @LifecyclePolicies@ array in the request body deletes any existing @LifecycleConfiguration@ and disables lifecycle management.
-- In the request, specify the following: 
--
--     * The ID for the file system for which you are enabling, disabling, or modifying lifecycle management.
--
--
--     * A @LifecyclePolicies@ array of @LifecyclePolicy@ objects that define when files are moved to the IA storage class. The array can contain only one @LifecyclePolicy@ item.
--
--
-- This operation requires permissions for the @elasticfilesystem:PutLifecycleConfiguration@ operation.
-- To apply a @LifecycleConfiguration@ object to an encrypted file system, you need the same AWS Key Management Service (AWS KMS) permissions as when you created the encrypted file system. 
module Network.AWS.EFS.PutLifecycleConfiguration
    (
    -- * Creating a request
      PutLifecycleConfiguration (..)
    , mkPutLifecycleConfiguration
    -- ** Request lenses
    , plcFileSystemId
    , plcLifecyclePolicies

     -- * Destructuring the response
    , Types.LifecycleConfigurationDescription (..)
    , Types.mkLifecycleConfigurationDescription
    -- ** Response lenses
    , Types.lcdLifecyclePolicies
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLifecycleConfiguration' smart constructor.
data PutLifecycleConfiguration = PutLifecycleConfiguration'
  { fileSystemId :: Types.FileSystemId
    -- ^ The ID of the file system for which you are creating the @LifecycleConfiguration@ object (String).
  , lifecyclePolicies :: [Types.LifecyclePolicy]
    -- ^ An array of @LifecyclePolicy@ objects that define the file system's @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells lifecycle management when to transition files from the Standard storage class to the Infrequent Access storage class.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecycleConfiguration' value with any optional fields omitted.
mkPutLifecycleConfiguration
    :: Types.FileSystemId -- ^ 'fileSystemId'
    -> PutLifecycleConfiguration
mkPutLifecycleConfiguration fileSystemId
  = PutLifecycleConfiguration'{fileSystemId,
                               lifecyclePolicies = Core.mempty}

-- | The ID of the file system for which you are creating the @LifecycleConfiguration@ object (String).
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcFileSystemId :: Lens.Lens' PutLifecycleConfiguration Types.FileSystemId
plcFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE plcFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | An array of @LifecyclePolicy@ objects that define the file system's @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells lifecycle management when to transition files from the Standard storage class to the Infrequent Access storage class.
--
-- /Note:/ Consider using 'lifecyclePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcLifecyclePolicies :: Lens.Lens' PutLifecycleConfiguration [Types.LifecyclePolicy]
plcLifecyclePolicies = Lens.field @"lifecyclePolicies"
{-# INLINEABLE plcLifecyclePolicies #-}
{-# DEPRECATED lifecyclePolicies "Use generic-lens or generic-optics with 'lifecyclePolicies' instead"  #-}

instance Core.ToQuery PutLifecycleConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutLifecycleConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON PutLifecycleConfiguration where
        toJSON PutLifecycleConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LifecyclePolicies" Core..= lifecyclePolicies)])

instance Core.AWSRequest PutLifecycleConfiguration where
        type Rs PutLifecycleConfiguration =
             Types.LifecycleConfigurationDescription
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2015-02-01/file-systems/" Core.<> Core.toText fileSystemId
                             Core.<> "/lifecycle-configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
