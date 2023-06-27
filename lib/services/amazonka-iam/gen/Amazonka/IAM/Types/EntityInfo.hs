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
-- Module      : Amazonka.IAM.Types.EntityInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.EntityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.PolicyOwnerEntityType
import qualified Amazonka.Prelude as Prelude

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
    path :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Text,
    -- | The name of the entity (user or role).
    name :: Prelude.Text,
    -- | The type of entity (user or role).
    type' :: PolicyOwnerEntityType,
    -- | The identifier of the entity (user or role).
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  PolicyOwnerEntityType ->
  -- | 'id'
  Prelude.Text ->
  EntityInfo
newEntityInfo pArn_ pName_ pType_ pId_ =
  EntityInfo'
    { path = Prelude.Nothing,
      arn = pArn_,
      name = pName_,
      type' = pType_,
      id = pId_
    }

-- | The path to the entity (user or role). For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
entityInfo_path :: Lens.Lens' EntityInfo (Prelude.Maybe Prelude.Text)
entityInfo_path = Lens.lens (\EntityInfo' {path} -> path) (\s@EntityInfo' {} a -> s {path = a} :: EntityInfo)

-- | Undocumented member.
entityInfo_arn :: Lens.Lens' EntityInfo Prelude.Text
entityInfo_arn = Lens.lens (\EntityInfo' {arn} -> arn) (\s@EntityInfo' {} a -> s {arn = a} :: EntityInfo)

-- | The name of the entity (user or role).
entityInfo_name :: Lens.Lens' EntityInfo Prelude.Text
entityInfo_name = Lens.lens (\EntityInfo' {name} -> name) (\s@EntityInfo' {} a -> s {name = a} :: EntityInfo)

-- | The type of entity (user or role).
entityInfo_type :: Lens.Lens' EntityInfo PolicyOwnerEntityType
entityInfo_type = Lens.lens (\EntityInfo' {type'} -> type') (\s@EntityInfo' {} a -> s {type' = a} :: EntityInfo)

-- | The identifier of the entity (user or role).
entityInfo_id :: Lens.Lens' EntityInfo Prelude.Text
entityInfo_id = Lens.lens (\EntityInfo' {id} -> id) (\s@EntityInfo' {} a -> s {id = a} :: EntityInfo)

instance Data.FromXML EntityInfo where
  parseXML x =
    EntityInfo'
      Prelude.<$> (x Data..@? "Path")
      Prelude.<*> (x Data..@ "Arn")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "Id")

instance Prelude.Hashable EntityInfo where
  hashWithSalt _salt EntityInfo' {..} =
    _salt
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData EntityInfo where
  rnf EntityInfo' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
