{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.EntityInfo
  ( EntityInfo (..)
  -- * Smart constructor
  , mkEntityInfo
  -- * Lenses
  , eiArn
  , eiName
  , eiType
  , eiId
  , eiPath
  ) where

import qualified Network.AWS.IAM.Types.ArnType as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.Name as Types
import qualified Network.AWS.IAM.Types.PathType as Types
import qualified Network.AWS.IAM.Types.PolicyOwnerEntityType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the specified entity (user or role).
--
-- This data type is an element of the 'EntityDetails' object.
--
-- /See:/ 'mkEntityInfo' smart constructor.
data EntityInfo = EntityInfo'
  { arn :: Types.ArnType
  , name :: Types.Name
    -- ^ The name of the entity (user or role).
  , type' :: Types.PolicyOwnerEntityType
    -- ^ The type of entity (user or role).
  , id :: Types.IdType
    -- ^ The identifier of the entity (user or role).
  , path :: Core.Maybe Types.PathType
    -- ^ The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntityInfo' value with any optional fields omitted.
mkEntityInfo
    :: Types.ArnType -- ^ 'arn'
    -> Types.Name -- ^ 'name'
    -> Types.PolicyOwnerEntityType -- ^ 'type\''
    -> Types.IdType -- ^ 'id'
    -> EntityInfo
mkEntityInfo arn name type' id
  = EntityInfo'{arn, name, type', id, path = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiArn :: Lens.Lens' EntityInfo Types.ArnType
eiArn = Lens.field @"arn"
{-# INLINEABLE eiArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the entity (user or role).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' EntityInfo Types.Name
eiName = Lens.field @"name"
{-# INLINEABLE eiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of entity (user or role).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiType :: Lens.Lens' EntityInfo Types.PolicyOwnerEntityType
eiType = Lens.field @"type'"
{-# INLINEABLE eiType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The identifier of the entity (user or role).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiId :: Lens.Lens' EntityInfo Types.IdType
eiId = Lens.field @"id"
{-# INLINEABLE eiId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiPath :: Lens.Lens' EntityInfo (Core.Maybe Types.PathType)
eiPath = Lens.field @"path"
{-# INLINEABLE eiPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.FromXML EntityInfo where
        parseXML x
          = EntityInfo' Core.<$>
              (x Core..@ "Arn") Core.<*> x Core..@ "Name" Core.<*>
                x Core..@ "Type"
                Core.<*> x Core..@ "Id"
                Core.<*> x Core..@? "Path"
