{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    eiARN,
    eiPath,
    eiName,
    eiId,
    eiType,
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
  { arn :: Lude.Text,
    -- | The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Lude.Maybe Lude.Text,
    -- | The name of the entity (user or role).
    name :: Lude.Text,
    -- | The identifier of the entity (user or role).
    id :: Lude.Text,
    -- | The type of entity (user or role).
    type' :: PolicyOwnerEntityType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityInfo' with the minimum fields required to make a request.
--
-- * 'arn' -
-- * 'path' - The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'name' - The name of the entity (user or role).
-- * 'id' - The identifier of the entity (user or role).
-- * 'type'' - The type of entity (user or role).
mkEntityInfo ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  PolicyOwnerEntityType ->
  EntityInfo
mkEntityInfo pARN_ pName_ pId_ pType_ =
  EntityInfo'
    { arn = pARN_,
      path = Lude.Nothing,
      name = pName_,
      id = pId_,
      type' = pType_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiARN :: Lens.Lens' EntityInfo Lude.Text
eiARN = Lens.lens (arn :: EntityInfo -> Lude.Text) (\s a -> s {arn = a} :: EntityInfo)
{-# DEPRECATED eiARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiPath :: Lens.Lens' EntityInfo (Lude.Maybe Lude.Text)
eiPath = Lens.lens (path :: EntityInfo -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: EntityInfo)
{-# DEPRECATED eiPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the entity (user or role).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' EntityInfo Lude.Text
eiName = Lens.lens (name :: EntityInfo -> Lude.Text) (\s a -> s {name = a} :: EntityInfo)
{-# DEPRECATED eiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the entity (user or role).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiId :: Lens.Lens' EntityInfo Lude.Text
eiId = Lens.lens (id :: EntityInfo -> Lude.Text) (\s a -> s {id = a} :: EntityInfo)
{-# DEPRECATED eiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of entity (user or role).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiType :: Lens.Lens' EntityInfo PolicyOwnerEntityType
eiType = Lens.lens (type' :: EntityInfo -> PolicyOwnerEntityType) (\s a -> s {type' = a} :: EntityInfo)
{-# DEPRECATED eiType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML EntityInfo where
  parseXML x =
    EntityInfo'
      Lude.<$> (x Lude..@ "Arn")
      Lude.<*> (x Lude..@? "Path")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "Type")
