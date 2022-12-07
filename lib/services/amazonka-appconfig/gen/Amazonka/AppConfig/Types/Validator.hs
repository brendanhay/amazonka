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
-- Module      : Amazonka.AppConfig.Types.Validator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Validator where

import Amazonka.AppConfig.Types.ValidatorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A validator provides a syntactic or semantic check to ensure the
-- configuration that you want to deploy functions as intended. To validate
-- your application configuration data, you provide a schema or an Amazon
-- Web Services Lambda function that runs against the configuration. The
-- configuration deployment or update can only proceed when the
-- configuration data is valid.
--
-- /See:/ 'newValidator' smart constructor.
data Validator = Validator'
  { -- | AppConfig supports validators of type @JSON_SCHEMA@ and @LAMBDA@
    type' :: ValidatorType,
    -- | Either the JSON Schema content or the Amazon Resource Name (ARN) of an
    -- Lambda function.
    content :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Validator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'validator_type' - AppConfig supports validators of type @JSON_SCHEMA@ and @LAMBDA@
--
-- 'content', 'validator_content' - Either the JSON Schema content or the Amazon Resource Name (ARN) of an
-- Lambda function.
newValidator ::
  -- | 'type''
  ValidatorType ->
  -- | 'content'
  Prelude.Text ->
  Validator
newValidator pType_ pContent_ =
  Validator'
    { type' = pType_,
      content = Data._Sensitive Lens.# pContent_
    }

-- | AppConfig supports validators of type @JSON_SCHEMA@ and @LAMBDA@
validator_type :: Lens.Lens' Validator ValidatorType
validator_type = Lens.lens (\Validator' {type'} -> type') (\s@Validator' {} a -> s {type' = a} :: Validator)

-- | Either the JSON Schema content or the Amazon Resource Name (ARN) of an
-- Lambda function.
validator_content :: Lens.Lens' Validator Prelude.Text
validator_content = Lens.lens (\Validator' {content} -> content) (\s@Validator' {} a -> s {content = a} :: Validator) Prelude.. Data._Sensitive

instance Data.FromJSON Validator where
  parseJSON =
    Data.withObject
      "Validator"
      ( \x ->
          Validator'
            Prelude.<$> (x Data..: "Type") Prelude.<*> (x Data..: "Content")
      )

instance Prelude.Hashable Validator where
  hashWithSalt _salt Validator' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` content

instance Prelude.NFData Validator where
  rnf Validator' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf content

instance Data.ToJSON Validator where
  toJSON Validator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Content" Data..= content)
          ]
      )
