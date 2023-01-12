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
-- Module      : Amazonka.Kendra.Types.EntityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.EntityConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.EntityType
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for users or groups in your IAM
-- Identity Center identity source to grant access your Amazon Kendra
-- experience.
--
-- /See:/ 'newEntityConfiguration' smart constructor.
data EntityConfiguration = EntityConfiguration'
  { -- | The identifier of a user or group in your IAM Identity Center identity
    -- source. For example, a user ID could be an email.
    entityId :: Prelude.Text,
    -- | Specifies whether you are configuring a @User@ or a @Group@.
    entityType :: EntityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'entityConfiguration_entityId' - The identifier of a user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
--
-- 'entityType', 'entityConfiguration_entityType' - Specifies whether you are configuring a @User@ or a @Group@.
newEntityConfiguration ::
  -- | 'entityId'
  Prelude.Text ->
  -- | 'entityType'
  EntityType ->
  EntityConfiguration
newEntityConfiguration pEntityId_ pEntityType_ =
  EntityConfiguration'
    { entityId = pEntityId_,
      entityType = pEntityType_
    }

-- | The identifier of a user or group in your IAM Identity Center identity
-- source. For example, a user ID could be an email.
entityConfiguration_entityId :: Lens.Lens' EntityConfiguration Prelude.Text
entityConfiguration_entityId = Lens.lens (\EntityConfiguration' {entityId} -> entityId) (\s@EntityConfiguration' {} a -> s {entityId = a} :: EntityConfiguration)

-- | Specifies whether you are configuring a @User@ or a @Group@.
entityConfiguration_entityType :: Lens.Lens' EntityConfiguration EntityType
entityConfiguration_entityType = Lens.lens (\EntityConfiguration' {entityType} -> entityType) (\s@EntityConfiguration' {} a -> s {entityType = a} :: EntityConfiguration)

instance Prelude.Hashable EntityConfiguration where
  hashWithSalt _salt EntityConfiguration' {..} =
    _salt `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` entityType

instance Prelude.NFData EntityConfiguration where
  rnf EntityConfiguration' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf entityType

instance Data.ToJSON EntityConfiguration where
  toJSON EntityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EntityId" Data..= entityId),
            Prelude.Just ("EntityType" Data..= entityType)
          ]
      )
