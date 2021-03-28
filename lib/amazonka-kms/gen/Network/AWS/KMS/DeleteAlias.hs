{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alias. You cannot perform this operation on an alias in a different AWS account. 
--
-- Because an alias is not a property of a CMK, you can delete and change the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases of all CMKs, use the 'ListAliases' operation. 
-- Each CMK can have multiple aliases. To change the alias of a CMK, use 'DeleteAlias' to delete the current alias and 'CreateAlias' to create a new alias. To associate an existing alias with a different customer master key (CMK), call 'UpdateAlias' .
module Network.AWS.KMS.DeleteAlias
    (
    -- * Creating a request
      DeleteAlias (..)
    , mkDeleteAlias
    -- ** Request lenses
    , daAliasName

    -- * Destructuring the response
    , DeleteAliasResponse (..)
    , mkDeleteAliasResponse
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAlias' smart constructor.
newtype DeleteAlias = DeleteAlias'
  { aliasName :: Types.AliasName
    -- ^ The alias to be deleted. The alias name must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlias' value with any optional fields omitted.
mkDeleteAlias
    :: Types.AliasName -- ^ 'aliasName'
    -> DeleteAlias
mkDeleteAlias aliasName = DeleteAlias'{aliasName}

-- | The alias to be deleted. The alias name must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ .
--
-- /Note:/ Consider using 'aliasName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAliasName :: Lens.Lens' DeleteAlias Types.AliasName
daAliasName = Lens.field @"aliasName"
{-# INLINEABLE daAliasName #-}
{-# DEPRECATED aliasName "Use generic-lens or generic-optics with 'aliasName' instead"  #-}

instance Core.ToQuery DeleteAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAlias where
        toHeaders DeleteAlias{..}
          = Core.pure ("X-Amz-Target", "TrentService.DeleteAlias") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAlias where
        toJSON DeleteAlias{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AliasName" Core..= aliasName)])

instance Core.AWSRequest DeleteAlias where
        type Rs DeleteAlias = DeleteAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAliasResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAliasResponse' value with any optional fields omitted.
mkDeleteAliasResponse
    :: DeleteAliasResponse
mkDeleteAliasResponse = DeleteAliasResponse'
