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
-- Module      : Amazonka.VerifiedPermissions.Types.EntitiesDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.EntitiesDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityItem

-- | Contains the list of entities to be considered during an authorization
-- request. This includes all principals, resources, and actions required
-- to successfully evaluate the request.
--
-- This data type is used as a field in the response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorized.html IsAuthorized>
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>
-- operations.
--
-- /See:/ 'newEntitiesDefinition' smart constructor.
data EntitiesDefinition = EntitiesDefinition'
  { -- | An array of entities that are needed to successfully evaluate an
    -- authorization request. Each entity in this array must include an
    -- identifier for the entity, the attributes of the entity, and a list of
    -- any parent entities.
    entityList :: Prelude.Maybe [EntityItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitiesDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityList', 'entitiesDefinition_entityList' - An array of entities that are needed to successfully evaluate an
-- authorization request. Each entity in this array must include an
-- identifier for the entity, the attributes of the entity, and a list of
-- any parent entities.
newEntitiesDefinition ::
  EntitiesDefinition
newEntitiesDefinition =
  EntitiesDefinition' {entityList = Prelude.Nothing}

-- | An array of entities that are needed to successfully evaluate an
-- authorization request. Each entity in this array must include an
-- identifier for the entity, the attributes of the entity, and a list of
-- any parent entities.
entitiesDefinition_entityList :: Lens.Lens' EntitiesDefinition (Prelude.Maybe [EntityItem])
entitiesDefinition_entityList = Lens.lens (\EntitiesDefinition' {entityList} -> entityList) (\s@EntitiesDefinition' {} a -> s {entityList = a} :: EntitiesDefinition) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EntitiesDefinition where
  hashWithSalt _salt EntitiesDefinition' {..} =
    _salt `Prelude.hashWithSalt` entityList

instance Prelude.NFData EntitiesDefinition where
  rnf EntitiesDefinition' {..} = Prelude.rnf entityList

instance Data.ToJSON EntitiesDefinition where
  toJSON EntitiesDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("entityList" Data..=) Prelude.<$> entityList]
      )
