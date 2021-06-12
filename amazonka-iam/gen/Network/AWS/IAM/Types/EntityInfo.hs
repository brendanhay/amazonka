{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.PolicyOwnerEntityType
import qualified Network.AWS.Lens as Lens

-- | Contains details about the specified entity (user or role).
--
-- This data type is an element of the EntityDetails object.
--
-- /See:/ 'newEntityInfo' smart constructor.
data EntityInfo = EntityInfo'
  { -- | The path to the entity (user or role). For more information about paths,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Maybe Core.Text,
    arn :: Core.Text,
    -- | The name of the entity (user or role).
    name :: Core.Text,
    -- | The type of entity (user or role).
    type' :: PolicyOwnerEntityType,
    -- | The identifier of the entity (user or role).
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'entityInfo_path' - The path to the entity (user or role). For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'arn', 'entityInfo_arn' - Undocumented member.
--
-- 'name', 'entityInfo_name' - The name of the entity (user or role).
--
-- 'type'', 'entityInfo_type' - The type of entity (user or role).
--
-- 'id', 'entityInfo_id' - The identifier of the entity (user or role).
newEntityInfo ::
  -- | 'arn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'type''
  PolicyOwnerEntityType ->
  -- | 'id'
  Core.Text ->
  EntityInfo
newEntityInfo pArn_ pName_ pType_ pId_ =
  EntityInfo'
    { path = Core.Nothing,
      arn = pArn_,
      name = pName_,
      type' = pType_,
      id = pId_
    }

-- | The path to the entity (user or role). For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
entityInfo_path :: Lens.Lens' EntityInfo (Core.Maybe Core.Text)
entityInfo_path = Lens.lens (\EntityInfo' {path} -> path) (\s@EntityInfo' {} a -> s {path = a} :: EntityInfo)

-- | Undocumented member.
entityInfo_arn :: Lens.Lens' EntityInfo Core.Text
entityInfo_arn = Lens.lens (\EntityInfo' {arn} -> arn) (\s@EntityInfo' {} a -> s {arn = a} :: EntityInfo)

-- | The name of the entity (user or role).
entityInfo_name :: Lens.Lens' EntityInfo Core.Text
entityInfo_name = Lens.lens (\EntityInfo' {name} -> name) (\s@EntityInfo' {} a -> s {name = a} :: EntityInfo)

-- | The type of entity (user or role).
entityInfo_type :: Lens.Lens' EntityInfo PolicyOwnerEntityType
entityInfo_type = Lens.lens (\EntityInfo' {type'} -> type') (\s@EntityInfo' {} a -> s {type' = a} :: EntityInfo)

-- | The identifier of the entity (user or role).
entityInfo_id :: Lens.Lens' EntityInfo Core.Text
entityInfo_id = Lens.lens (\EntityInfo' {id} -> id) (\s@EntityInfo' {} a -> s {id = a} :: EntityInfo)

instance Core.FromXML EntityInfo where
  parseXML x =
    EntityInfo'
      Core.<$> (x Core..@? "Path")
      Core.<*> (x Core..@ "Arn")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "Type")
      Core.<*> (x Core..@ "Id")

instance Core.Hashable EntityInfo

instance Core.NFData EntityInfo
