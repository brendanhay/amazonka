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
-- Module      : Amazonka.Greengrass.Types.Function
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.Function where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.FunctionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Information about a Lambda function.
--
-- /See:/ 'newFunction' smart constructor.
data Function = Function'
  { -- | The ARN of the Lambda function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the Lambda function.
    functionConfiguration :: Prelude.Maybe FunctionConfiguration,
    -- | A descriptive or arbitrary ID for the function. This value must be
    -- unique within the function definition version. Max length is 128
    -- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Function' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionArn', 'function_functionArn' - The ARN of the Lambda function.
--
-- 'functionConfiguration', 'function_functionConfiguration' - The configuration of the Lambda function.
--
-- 'id', 'function_id' - A descriptive or arbitrary ID for the function. This value must be
-- unique within the function definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
newFunction ::
  -- | 'id'
  Prelude.Text ->
  Function
newFunction pId_ =
  Function'
    { functionArn = Prelude.Nothing,
      functionConfiguration = Prelude.Nothing,
      id = pId_
    }

-- | The ARN of the Lambda function.
function_functionArn :: Lens.Lens' Function (Prelude.Maybe Prelude.Text)
function_functionArn = Lens.lens (\Function' {functionArn} -> functionArn) (\s@Function' {} a -> s {functionArn = a} :: Function)

-- | The configuration of the Lambda function.
function_functionConfiguration :: Lens.Lens' Function (Prelude.Maybe FunctionConfiguration)
function_functionConfiguration = Lens.lens (\Function' {functionConfiguration} -> functionConfiguration) (\s@Function' {} a -> s {functionConfiguration = a} :: Function)

-- | A descriptive or arbitrary ID for the function. This value must be
-- unique within the function definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
function_id :: Lens.Lens' Function Prelude.Text
function_id = Lens.lens (\Function' {id} -> id) (\s@Function' {} a -> s {id = a} :: Function)

instance Data.FromJSON Function where
  parseJSON =
    Data.withObject
      "Function"
      ( \x ->
          Function'
            Prelude.<$> (x Data..:? "FunctionArn")
            Prelude.<*> (x Data..:? "FunctionConfiguration")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable Function where
  hashWithSalt _salt Function' {..} =
    _salt
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` functionConfiguration
      `Prelude.hashWithSalt` id

instance Prelude.NFData Function where
  rnf Function' {..} =
    Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf functionConfiguration
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON Function where
  toJSON Function' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FunctionArn" Data..=) Prelude.<$> functionArn,
            ("FunctionConfiguration" Data..=)
              Prelude.<$> functionConfiguration,
            Prelude.Just ("Id" Data..= id)
          ]
      )
