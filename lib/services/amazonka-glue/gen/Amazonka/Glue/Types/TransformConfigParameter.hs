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
-- Module      : Amazonka.Glue.Types.TransformConfigParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TransformConfigParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ParamType
import qualified Amazonka.Prelude as Prelude

-- | Specifies the parameters in the config file of the dynamic transform.
--
-- /See:/ 'newTransformConfigParameter' smart constructor.
data TransformConfigParameter = TransformConfigParameter'
  { -- | Specifies whether the parameter is optional or not in the config file of
    -- the dynamic transform.
    isOptional :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the list type of the parameter in the config file of the
    -- dynamic transform.
    listType :: Prelude.Maybe ParamType,
    -- | Specifies the validation message in the config file of the dynamic
    -- transform.
    validationMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the validation rule in the config file of the dynamic
    -- transform.
    validationRule :: Prelude.Maybe Prelude.Text,
    -- | Specifies the value of the parameter in the config file of the dynamic
    -- transform.
    value :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the name of the parameter in the config file of the dynamic
    -- transform.
    name :: Prelude.Text,
    -- | Specifies the parameter type in the config file of the dynamic
    -- transform.
    type' :: ParamType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformConfigParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isOptional', 'transformConfigParameter_isOptional' - Specifies whether the parameter is optional or not in the config file of
-- the dynamic transform.
--
-- 'listType', 'transformConfigParameter_listType' - Specifies the list type of the parameter in the config file of the
-- dynamic transform.
--
-- 'validationMessage', 'transformConfigParameter_validationMessage' - Specifies the validation message in the config file of the dynamic
-- transform.
--
-- 'validationRule', 'transformConfigParameter_validationRule' - Specifies the validation rule in the config file of the dynamic
-- transform.
--
-- 'value', 'transformConfigParameter_value' - Specifies the value of the parameter in the config file of the dynamic
-- transform.
--
-- 'name', 'transformConfigParameter_name' - Specifies the name of the parameter in the config file of the dynamic
-- transform.
--
-- 'type'', 'transformConfigParameter_type' - Specifies the parameter type in the config file of the dynamic
-- transform.
newTransformConfigParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ParamType ->
  TransformConfigParameter
newTransformConfigParameter pName_ pType_ =
  TransformConfigParameter'
    { isOptional =
        Prelude.Nothing,
      listType = Prelude.Nothing,
      validationMessage = Prelude.Nothing,
      validationRule = Prelude.Nothing,
      value = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Specifies whether the parameter is optional or not in the config file of
-- the dynamic transform.
transformConfigParameter_isOptional :: Lens.Lens' TransformConfigParameter (Prelude.Maybe Prelude.Bool)
transformConfigParameter_isOptional = Lens.lens (\TransformConfigParameter' {isOptional} -> isOptional) (\s@TransformConfigParameter' {} a -> s {isOptional = a} :: TransformConfigParameter)

-- | Specifies the list type of the parameter in the config file of the
-- dynamic transform.
transformConfigParameter_listType :: Lens.Lens' TransformConfigParameter (Prelude.Maybe ParamType)
transformConfigParameter_listType = Lens.lens (\TransformConfigParameter' {listType} -> listType) (\s@TransformConfigParameter' {} a -> s {listType = a} :: TransformConfigParameter)

-- | Specifies the validation message in the config file of the dynamic
-- transform.
transformConfigParameter_validationMessage :: Lens.Lens' TransformConfigParameter (Prelude.Maybe Prelude.Text)
transformConfigParameter_validationMessage = Lens.lens (\TransformConfigParameter' {validationMessage} -> validationMessage) (\s@TransformConfigParameter' {} a -> s {validationMessage = a} :: TransformConfigParameter)

-- | Specifies the validation rule in the config file of the dynamic
-- transform.
transformConfigParameter_validationRule :: Lens.Lens' TransformConfigParameter (Prelude.Maybe Prelude.Text)
transformConfigParameter_validationRule = Lens.lens (\TransformConfigParameter' {validationRule} -> validationRule) (\s@TransformConfigParameter' {} a -> s {validationRule = a} :: TransformConfigParameter)

-- | Specifies the value of the parameter in the config file of the dynamic
-- transform.
transformConfigParameter_value :: Lens.Lens' TransformConfigParameter (Prelude.Maybe [Prelude.Text])
transformConfigParameter_value = Lens.lens (\TransformConfigParameter' {value} -> value) (\s@TransformConfigParameter' {} a -> s {value = a} :: TransformConfigParameter) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the parameter in the config file of the dynamic
-- transform.
transformConfigParameter_name :: Lens.Lens' TransformConfigParameter Prelude.Text
transformConfigParameter_name = Lens.lens (\TransformConfigParameter' {name} -> name) (\s@TransformConfigParameter' {} a -> s {name = a} :: TransformConfigParameter)

-- | Specifies the parameter type in the config file of the dynamic
-- transform.
transformConfigParameter_type :: Lens.Lens' TransformConfigParameter ParamType
transformConfigParameter_type = Lens.lens (\TransformConfigParameter' {type'} -> type') (\s@TransformConfigParameter' {} a -> s {type' = a} :: TransformConfigParameter)

instance Data.FromJSON TransformConfigParameter where
  parseJSON =
    Data.withObject
      "TransformConfigParameter"
      ( \x ->
          TransformConfigParameter'
            Prelude.<$> (x Data..:? "IsOptional")
            Prelude.<*> (x Data..:? "ListType")
            Prelude.<*> (x Data..:? "ValidationMessage")
            Prelude.<*> (x Data..:? "ValidationRule")
            Prelude.<*> (x Data..:? "Value" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable TransformConfigParameter where
  hashWithSalt _salt TransformConfigParameter' {..} =
    _salt
      `Prelude.hashWithSalt` isOptional
      `Prelude.hashWithSalt` listType
      `Prelude.hashWithSalt` validationMessage
      `Prelude.hashWithSalt` validationRule
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TransformConfigParameter where
  rnf TransformConfigParameter' {..} =
    Prelude.rnf isOptional
      `Prelude.seq` Prelude.rnf listType
      `Prelude.seq` Prelude.rnf validationMessage
      `Prelude.seq` Prelude.rnf validationRule
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON TransformConfigParameter where
  toJSON TransformConfigParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsOptional" Data..=) Prelude.<$> isOptional,
            ("ListType" Data..=) Prelude.<$> listType,
            ("ValidationMessage" Data..=)
              Prelude.<$> validationMessage,
            ("ValidationRule" Data..=)
              Prelude.<$> validationRule,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type')
          ]
      )
