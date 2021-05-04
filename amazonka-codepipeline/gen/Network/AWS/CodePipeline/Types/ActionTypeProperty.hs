{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeProperty where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about each property specified in the action
-- configuration, such as the description and key name that display for the
-- customer using the action type.
--
-- /See:/ 'newActionTypeProperty' smart constructor.
data ActionTypeProperty = ActionTypeProperty'
  { -- | The description of the property that is displayed to users.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates that the property is used with polling. An action type can
    -- have up to one queryable property. If it has one, that property must be
    -- both required and not secret.
    queryable :: Prelude.Maybe Prelude.Bool,
    -- | The property name that is displayed to users.
    name :: Prelude.Text,
    -- | Whether the configuration property is an optional value.
    optional :: Prelude.Bool,
    -- | Whether the configuration property is a key.
    key :: Prelude.Bool,
    -- | Whether to omit the field value entered by the customer in the log. If
    -- @true@, the value is not saved in CloudTrail logs for the action
    -- execution.
    noEcho :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'actionTypeProperty_description' - The description of the property that is displayed to users.
--
-- 'queryable', 'actionTypeProperty_queryable' - Indicates that the property is used with polling. An action type can
-- have up to one queryable property. If it has one, that property must be
-- both required and not secret.
--
-- 'name', 'actionTypeProperty_name' - The property name that is displayed to users.
--
-- 'optional', 'actionTypeProperty_optional' - Whether the configuration property is an optional value.
--
-- 'key', 'actionTypeProperty_key' - Whether the configuration property is a key.
--
-- 'noEcho', 'actionTypeProperty_noEcho' - Whether to omit the field value entered by the customer in the log. If
-- @true@, the value is not saved in CloudTrail logs for the action
-- execution.
newActionTypeProperty ::
  -- | 'name'
  Prelude.Text ->
  -- | 'optional'
  Prelude.Bool ->
  -- | 'key'
  Prelude.Bool ->
  -- | 'noEcho'
  Prelude.Bool ->
  ActionTypeProperty
newActionTypeProperty
  pName_
  pOptional_
  pKey_
  pNoEcho_ =
    ActionTypeProperty'
      { description = Prelude.Nothing,
        queryable = Prelude.Nothing,
        name = pName_,
        optional = pOptional_,
        key = pKey_,
        noEcho = pNoEcho_
      }

-- | The description of the property that is displayed to users.
actionTypeProperty_description :: Lens.Lens' ActionTypeProperty (Prelude.Maybe Prelude.Text)
actionTypeProperty_description = Lens.lens (\ActionTypeProperty' {description} -> description) (\s@ActionTypeProperty' {} a -> s {description = a} :: ActionTypeProperty)

-- | Indicates that the property is used with polling. An action type can
-- have up to one queryable property. If it has one, that property must be
-- both required and not secret.
actionTypeProperty_queryable :: Lens.Lens' ActionTypeProperty (Prelude.Maybe Prelude.Bool)
actionTypeProperty_queryable = Lens.lens (\ActionTypeProperty' {queryable} -> queryable) (\s@ActionTypeProperty' {} a -> s {queryable = a} :: ActionTypeProperty)

-- | The property name that is displayed to users.
actionTypeProperty_name :: Lens.Lens' ActionTypeProperty Prelude.Text
actionTypeProperty_name = Lens.lens (\ActionTypeProperty' {name} -> name) (\s@ActionTypeProperty' {} a -> s {name = a} :: ActionTypeProperty)

-- | Whether the configuration property is an optional value.
actionTypeProperty_optional :: Lens.Lens' ActionTypeProperty Prelude.Bool
actionTypeProperty_optional = Lens.lens (\ActionTypeProperty' {optional} -> optional) (\s@ActionTypeProperty' {} a -> s {optional = a} :: ActionTypeProperty)

-- | Whether the configuration property is a key.
actionTypeProperty_key :: Lens.Lens' ActionTypeProperty Prelude.Bool
actionTypeProperty_key = Lens.lens (\ActionTypeProperty' {key} -> key) (\s@ActionTypeProperty' {} a -> s {key = a} :: ActionTypeProperty)

-- | Whether to omit the field value entered by the customer in the log. If
-- @true@, the value is not saved in CloudTrail logs for the action
-- execution.
actionTypeProperty_noEcho :: Lens.Lens' ActionTypeProperty Prelude.Bool
actionTypeProperty_noEcho = Lens.lens (\ActionTypeProperty' {noEcho} -> noEcho) (\s@ActionTypeProperty' {} a -> s {noEcho = a} :: ActionTypeProperty)

instance Prelude.FromJSON ActionTypeProperty where
  parseJSON =
    Prelude.withObject
      "ActionTypeProperty"
      ( \x ->
          ActionTypeProperty'
            Prelude.<$> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "queryable")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "optional")
            Prelude.<*> (x Prelude..: "key")
            Prelude.<*> (x Prelude..: "noEcho")
      )

instance Prelude.Hashable ActionTypeProperty

instance Prelude.NFData ActionTypeProperty

instance Prelude.ToJSON ActionTypeProperty where
  toJSON ActionTypeProperty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("description" Prelude..=) Prelude.<$> description,
            ("queryable" Prelude..=) Prelude.<$> queryable,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("optional" Prelude..= optional),
            Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("noEcho" Prelude..= noEcho)
          ]
      )
