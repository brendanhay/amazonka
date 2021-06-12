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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about each property specified in the action
-- configuration, such as the description and key name that display for the
-- customer using the action type.
--
-- /See:/ 'newActionTypeProperty' smart constructor.
data ActionTypeProperty = ActionTypeProperty'
  { -- | The description of the property that is displayed to users.
    description :: Core.Maybe Core.Text,
    -- | Indicates that the property is used with polling. An action type can
    -- have up to one queryable property. If it has one, that property must be
    -- both required and not secret.
    queryable :: Core.Maybe Core.Bool,
    -- | The property name that is displayed to users.
    name :: Core.Text,
    -- | Whether the configuration property is an optional value.
    optional :: Core.Bool,
    -- | Whether the configuration property is a key.
    key :: Core.Bool,
    -- | Whether to omit the field value entered by the customer in the log. If
    -- @true@, the value is not saved in CloudTrail logs for the action
    -- execution.
    noEcho :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'optional'
  Core.Bool ->
  -- | 'key'
  Core.Bool ->
  -- | 'noEcho'
  Core.Bool ->
  ActionTypeProperty
newActionTypeProperty
  pName_
  pOptional_
  pKey_
  pNoEcho_ =
    ActionTypeProperty'
      { description = Core.Nothing,
        queryable = Core.Nothing,
        name = pName_,
        optional = pOptional_,
        key = pKey_,
        noEcho = pNoEcho_
      }

-- | The description of the property that is displayed to users.
actionTypeProperty_description :: Lens.Lens' ActionTypeProperty (Core.Maybe Core.Text)
actionTypeProperty_description = Lens.lens (\ActionTypeProperty' {description} -> description) (\s@ActionTypeProperty' {} a -> s {description = a} :: ActionTypeProperty)

-- | Indicates that the property is used with polling. An action type can
-- have up to one queryable property. If it has one, that property must be
-- both required and not secret.
actionTypeProperty_queryable :: Lens.Lens' ActionTypeProperty (Core.Maybe Core.Bool)
actionTypeProperty_queryable = Lens.lens (\ActionTypeProperty' {queryable} -> queryable) (\s@ActionTypeProperty' {} a -> s {queryable = a} :: ActionTypeProperty)

-- | The property name that is displayed to users.
actionTypeProperty_name :: Lens.Lens' ActionTypeProperty Core.Text
actionTypeProperty_name = Lens.lens (\ActionTypeProperty' {name} -> name) (\s@ActionTypeProperty' {} a -> s {name = a} :: ActionTypeProperty)

-- | Whether the configuration property is an optional value.
actionTypeProperty_optional :: Lens.Lens' ActionTypeProperty Core.Bool
actionTypeProperty_optional = Lens.lens (\ActionTypeProperty' {optional} -> optional) (\s@ActionTypeProperty' {} a -> s {optional = a} :: ActionTypeProperty)

-- | Whether the configuration property is a key.
actionTypeProperty_key :: Lens.Lens' ActionTypeProperty Core.Bool
actionTypeProperty_key = Lens.lens (\ActionTypeProperty' {key} -> key) (\s@ActionTypeProperty' {} a -> s {key = a} :: ActionTypeProperty)

-- | Whether to omit the field value entered by the customer in the log. If
-- @true@, the value is not saved in CloudTrail logs for the action
-- execution.
actionTypeProperty_noEcho :: Lens.Lens' ActionTypeProperty Core.Bool
actionTypeProperty_noEcho = Lens.lens (\ActionTypeProperty' {noEcho} -> noEcho) (\s@ActionTypeProperty' {} a -> s {noEcho = a} :: ActionTypeProperty)

instance Core.FromJSON ActionTypeProperty where
  parseJSON =
    Core.withObject
      "ActionTypeProperty"
      ( \x ->
          ActionTypeProperty'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..:? "queryable")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "optional")
            Core.<*> (x Core..: "key")
            Core.<*> (x Core..: "noEcho")
      )

instance Core.Hashable ActionTypeProperty

instance Core.NFData ActionTypeProperty

instance Core.ToJSON ActionTypeProperty where
  toJSON ActionTypeProperty' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            ("queryable" Core..=) Core.<$> queryable,
            Core.Just ("name" Core..= name),
            Core.Just ("optional" Core..= optional),
            Core.Just ("key" Core..= key),
            Core.Just ("noEcho" Core..= noEcho)
          ]
      )
