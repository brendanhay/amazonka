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
-- Module      : Amazonka.CodePipeline.Types.ActionConfigurationProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionConfigurationProperty where

import Amazonka.CodePipeline.Types.ActionConfigurationPropertyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an action configuration property.
--
-- /See:/ 'newActionConfigurationProperty' smart constructor.
data ActionConfigurationProperty = ActionConfigurationProperty'
  { -- | The description of the action configuration property that is displayed
    -- to users.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates that the property is used with @PollForJobs@. When creating a
    -- custom action, an action can have up to one queryable property. If it
    -- has one, that property must be both required and not secret.
    --
    -- If you create a pipeline with a custom action type, and that custom
    -- action contains a queryable property, the value for that configuration
    -- property is subject to other restrictions. The value must be less than
    -- or equal to twenty (20) characters. The value can contain only
    -- alphanumeric characters, underscores, and hyphens.
    queryable :: Prelude.Maybe Prelude.Bool,
    -- | The type of the configuration property.
    type' :: Prelude.Maybe ActionConfigurationPropertyType,
    -- | The name of the action configuration property.
    name :: Prelude.Text,
    -- | Whether the configuration property is a required value.
    required :: Prelude.Bool,
    -- | Whether the configuration property is a key.
    key :: Prelude.Bool,
    -- | Whether the configuration property is secret. Secrets are hidden from
    -- all calls except for @GetJobDetails@, @GetThirdPartyJobDetails@,
    -- @PollForJobs@, and @PollForThirdPartyJobs@.
    --
    -- When updating a pipeline, passing * * * * * without changing any other
    -- values of the action preserves the previous value of the secret.
    secret :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionConfigurationProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'actionConfigurationProperty_description' - The description of the action configuration property that is displayed
-- to users.
--
-- 'queryable', 'actionConfigurationProperty_queryable' - Indicates that the property is used with @PollForJobs@. When creating a
-- custom action, an action can have up to one queryable property. If it
-- has one, that property must be both required and not secret.
--
-- If you create a pipeline with a custom action type, and that custom
-- action contains a queryable property, the value for that configuration
-- property is subject to other restrictions. The value must be less than
-- or equal to twenty (20) characters. The value can contain only
-- alphanumeric characters, underscores, and hyphens.
--
-- 'type'', 'actionConfigurationProperty_type' - The type of the configuration property.
--
-- 'name', 'actionConfigurationProperty_name' - The name of the action configuration property.
--
-- 'required', 'actionConfigurationProperty_required' - Whether the configuration property is a required value.
--
-- 'key', 'actionConfigurationProperty_key' - Whether the configuration property is a key.
--
-- 'secret', 'actionConfigurationProperty_secret' - Whether the configuration property is secret. Secrets are hidden from
-- all calls except for @GetJobDetails@, @GetThirdPartyJobDetails@,
-- @PollForJobs@, and @PollForThirdPartyJobs@.
--
-- When updating a pipeline, passing * * * * * without changing any other
-- values of the action preserves the previous value of the secret.
newActionConfigurationProperty ::
  -- | 'name'
  Prelude.Text ->
  -- | 'required'
  Prelude.Bool ->
  -- | 'key'
  Prelude.Bool ->
  -- | 'secret'
  Prelude.Bool ->
  ActionConfigurationProperty
newActionConfigurationProperty
  pName_
  pRequired_
  pKey_
  pSecret_ =
    ActionConfigurationProperty'
      { description =
          Prelude.Nothing,
        queryable = Prelude.Nothing,
        type' = Prelude.Nothing,
        name = pName_,
        required = pRequired_,
        key = pKey_,
        secret = pSecret_
      }

-- | The description of the action configuration property that is displayed
-- to users.
actionConfigurationProperty_description :: Lens.Lens' ActionConfigurationProperty (Prelude.Maybe Prelude.Text)
actionConfigurationProperty_description = Lens.lens (\ActionConfigurationProperty' {description} -> description) (\s@ActionConfigurationProperty' {} a -> s {description = a} :: ActionConfigurationProperty)

-- | Indicates that the property is used with @PollForJobs@. When creating a
-- custom action, an action can have up to one queryable property. If it
-- has one, that property must be both required and not secret.
--
-- If you create a pipeline with a custom action type, and that custom
-- action contains a queryable property, the value for that configuration
-- property is subject to other restrictions. The value must be less than
-- or equal to twenty (20) characters. The value can contain only
-- alphanumeric characters, underscores, and hyphens.
actionConfigurationProperty_queryable :: Lens.Lens' ActionConfigurationProperty (Prelude.Maybe Prelude.Bool)
actionConfigurationProperty_queryable = Lens.lens (\ActionConfigurationProperty' {queryable} -> queryable) (\s@ActionConfigurationProperty' {} a -> s {queryable = a} :: ActionConfigurationProperty)

-- | The type of the configuration property.
actionConfigurationProperty_type :: Lens.Lens' ActionConfigurationProperty (Prelude.Maybe ActionConfigurationPropertyType)
actionConfigurationProperty_type = Lens.lens (\ActionConfigurationProperty' {type'} -> type') (\s@ActionConfigurationProperty' {} a -> s {type' = a} :: ActionConfigurationProperty)

-- | The name of the action configuration property.
actionConfigurationProperty_name :: Lens.Lens' ActionConfigurationProperty Prelude.Text
actionConfigurationProperty_name = Lens.lens (\ActionConfigurationProperty' {name} -> name) (\s@ActionConfigurationProperty' {} a -> s {name = a} :: ActionConfigurationProperty)

-- | Whether the configuration property is a required value.
actionConfigurationProperty_required :: Lens.Lens' ActionConfigurationProperty Prelude.Bool
actionConfigurationProperty_required = Lens.lens (\ActionConfigurationProperty' {required} -> required) (\s@ActionConfigurationProperty' {} a -> s {required = a} :: ActionConfigurationProperty)

-- | Whether the configuration property is a key.
actionConfigurationProperty_key :: Lens.Lens' ActionConfigurationProperty Prelude.Bool
actionConfigurationProperty_key = Lens.lens (\ActionConfigurationProperty' {key} -> key) (\s@ActionConfigurationProperty' {} a -> s {key = a} :: ActionConfigurationProperty)

-- | Whether the configuration property is secret. Secrets are hidden from
-- all calls except for @GetJobDetails@, @GetThirdPartyJobDetails@,
-- @PollForJobs@, and @PollForThirdPartyJobs@.
--
-- When updating a pipeline, passing * * * * * without changing any other
-- values of the action preserves the previous value of the secret.
actionConfigurationProperty_secret :: Lens.Lens' ActionConfigurationProperty Prelude.Bool
actionConfigurationProperty_secret = Lens.lens (\ActionConfigurationProperty' {secret} -> secret) (\s@ActionConfigurationProperty' {} a -> s {secret = a} :: ActionConfigurationProperty)

instance Data.FromJSON ActionConfigurationProperty where
  parseJSON =
    Data.withObject
      "ActionConfigurationProperty"
      ( \x ->
          ActionConfigurationProperty'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "queryable")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "required")
            Prelude.<*> (x Data..: "key")
            Prelude.<*> (x Data..: "secret")
      )

instance Prelude.Hashable ActionConfigurationProperty where
  hashWithSalt _salt ActionConfigurationProperty' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` queryable
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` required
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` secret

instance Prelude.NFData ActionConfigurationProperty where
  rnf ActionConfigurationProperty' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf queryable
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf required
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf secret

instance Data.ToJSON ActionConfigurationProperty where
  toJSON ActionConfigurationProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("queryable" Data..=) Prelude.<$> queryable,
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("required" Data..= required),
            Prelude.Just ("key" Data..= key),
            Prelude.Just ("secret" Data..= secret)
          ]
      )
