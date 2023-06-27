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
-- Module      : Amazonka.VerifiedPermissions.Types.EntityIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.EntityIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the identifier of an entity, including its ID and type.
--
-- This data type is used as a request parameter for
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorized.html IsAuthorized>
-- operation, and as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>,
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_GetPolicy.html GetPolicy>,
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicy.html UpdatePolicy>
-- operations.
--
-- Example:
-- @{\"entityId\":\"@/@string@/@\",\"entityType\":\"@/@string@/@\"}@
--
-- /See:/ 'newEntityIdentifier' smart constructor.
data EntityIdentifier = EntityIdentifier'
  { -- | The type of an entity.
    --
    -- Example: @\"entityType\":\"@/@typeName@/@\"@
    entityType :: Prelude.Text,
    -- | The identifier of an entity.
    --
    -- @\"entityId\":\"@/@identifier@/@\"@
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityType', 'entityIdentifier_entityType' - The type of an entity.
--
-- Example: @\"entityType\":\"@/@typeName@/@\"@
--
-- 'entityId', 'entityIdentifier_entityId' - The identifier of an entity.
--
-- @\"entityId\":\"@/@identifier@/@\"@
newEntityIdentifier ::
  -- | 'entityType'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  EntityIdentifier
newEntityIdentifier pEntityType_ pEntityId_ =
  EntityIdentifier'
    { entityType = pEntityType_,
      entityId = pEntityId_
    }

-- | The type of an entity.
--
-- Example: @\"entityType\":\"@/@typeName@/@\"@
entityIdentifier_entityType :: Lens.Lens' EntityIdentifier Prelude.Text
entityIdentifier_entityType = Lens.lens (\EntityIdentifier' {entityType} -> entityType) (\s@EntityIdentifier' {} a -> s {entityType = a} :: EntityIdentifier)

-- | The identifier of an entity.
--
-- @\"entityId\":\"@/@identifier@/@\"@
entityIdentifier_entityId :: Lens.Lens' EntityIdentifier Prelude.Text
entityIdentifier_entityId = Lens.lens (\EntityIdentifier' {entityId} -> entityId) (\s@EntityIdentifier' {} a -> s {entityId = a} :: EntityIdentifier)

instance Data.FromJSON EntityIdentifier where
  parseJSON =
    Data.withObject
      "EntityIdentifier"
      ( \x ->
          EntityIdentifier'
            Prelude.<$> (x Data..: "entityType")
            Prelude.<*> (x Data..: "entityId")
      )

instance Prelude.Hashable EntityIdentifier where
  hashWithSalt _salt EntityIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` entityType
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData EntityIdentifier where
  rnf EntityIdentifier' {..} =
    Prelude.rnf entityType
      `Prelude.seq` Prelude.rnf entityId

instance Data.ToJSON EntityIdentifier where
  toJSON EntityIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("entityType" Data..= entityType),
            Prelude.Just ("entityId" Data..= entityId)
          ]
      )
