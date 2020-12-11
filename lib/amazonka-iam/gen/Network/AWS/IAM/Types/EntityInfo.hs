-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityInfo
  ( EntityInfo (..),

    -- * Smart constructor
    mkEntityInfo,

    -- * Lenses
    eiPath,
    eiARN,
    eiName,
    eiType,
    eiId,
  )
where

import Network.AWS.IAM.Types.PolicyOwnerEntityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the specified entity (user or role).
--
-- This data type is an element of the 'EntityDetails' object.
--
-- /See:/ 'mkEntityInfo' smart constructor.
data EntityInfo = EntityInfo'
  { path :: Lude.Maybe Lude.Text,
    arn :: Lude.Text,
    name :: Lude.Text,
    type' :: PolicyOwnerEntityType,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityInfo' with the minimum fields required to make a request.
--
-- * 'arn' - Undocumented field.
-- * 'id' - The identifier of the entity (user or role).
-- * 'name' - The name of the entity (user or role).
-- * 'path' - The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'type'' - The type of entity (user or role).
mkEntityInfo ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  PolicyOwnerEntityType ->
  -- | 'id'
  Lude.Text ->
  EntityInfo
mkEntityInfo pARN_ pName_ pType_ pId_ =
  EntityInfo'
    { path = Lude.Nothing,
      arn = pARN_,
      name = pName_,
      type' = pType_,
      id = pId_
    }

-- | The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiPath :: Lens.Lens' EntityInfo (Lude.Maybe Lude.Text)
eiPath = Lens.lens (path :: EntityInfo -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: EntityInfo)
{-# DEPRECATED eiPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiARN :: Lens.Lens' EntityInfo Lude.Text
eiARN = Lens.lens (arn :: EntityInfo -> Lude.Text) (\s a -> s {arn = a} :: EntityInfo)
{-# DEPRECATED eiARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the entity (user or role).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' EntityInfo Lude.Text
eiName = Lens.lens (name :: EntityInfo -> Lude.Text) (\s a -> s {name = a} :: EntityInfo)
{-# DEPRECATED eiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of entity (user or role).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiType :: Lens.Lens' EntityInfo PolicyOwnerEntityType
eiType = Lens.lens (type' :: EntityInfo -> PolicyOwnerEntityType) (\s a -> s {type' = a} :: EntityInfo)
{-# DEPRECATED eiType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The identifier of the entity (user or role).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiId :: Lens.Lens' EntityInfo Lude.Text
eiId = Lens.lens (id :: EntityInfo -> Lude.Text) (\s a -> s {id = a} :: EntityInfo)
{-# DEPRECATED eiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML EntityInfo where
  parseXML x =
    EntityInfo'
      Lude.<$> (x Lude..@? "Path")
      Lude.<*> (x Lude..@ "Arn")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "Type")
      Lude.<*> (x Lude..@ "Id")
