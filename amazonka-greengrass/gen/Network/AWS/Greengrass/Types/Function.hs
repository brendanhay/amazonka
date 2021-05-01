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
-- Module      : Network.AWS.Greengrass.Types.Function
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Function where

import Network.AWS.Greengrass.Types.FunctionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Lambda function.
--
-- /See:/ 'newFunction' smart constructor.
data Function = Function'
  { -- | The configuration of the Lambda function.
    functionConfiguration :: Prelude.Maybe FunctionConfiguration,
    -- | The ARN of the Lambda function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | A descriptive or arbitrary ID for the function. This value must be
    -- unique within the function definition version. Max length is 128
    -- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Function' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionConfiguration', 'function_functionConfiguration' - The configuration of the Lambda function.
--
-- 'functionArn', 'function_functionArn' - The ARN of the Lambda function.
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
    { functionConfiguration = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      id = pId_
    }

-- | The configuration of the Lambda function.
function_functionConfiguration :: Lens.Lens' Function (Prelude.Maybe FunctionConfiguration)
function_functionConfiguration = Lens.lens (\Function' {functionConfiguration} -> functionConfiguration) (\s@Function' {} a -> s {functionConfiguration = a} :: Function)

-- | The ARN of the Lambda function.
function_functionArn :: Lens.Lens' Function (Prelude.Maybe Prelude.Text)
function_functionArn = Lens.lens (\Function' {functionArn} -> functionArn) (\s@Function' {} a -> s {functionArn = a} :: Function)

-- | A descriptive or arbitrary ID for the function. This value must be
-- unique within the function definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
function_id :: Lens.Lens' Function Prelude.Text
function_id = Lens.lens (\Function' {id} -> id) (\s@Function' {} a -> s {id = a} :: Function)

instance Prelude.FromJSON Function where
  parseJSON =
    Prelude.withObject
      "Function"
      ( \x ->
          Function'
            Prelude.<$> (x Prelude..:? "FunctionConfiguration")
            Prelude.<*> (x Prelude..:? "FunctionArn")
            Prelude.<*> (x Prelude..: "Id")
      )

instance Prelude.Hashable Function

instance Prelude.NFData Function

instance Prelude.ToJSON Function where
  toJSON Function' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FunctionConfiguration" Prelude..=)
              Prelude.<$> functionConfiguration,
            ("FunctionArn" Prelude..=) Prelude.<$> functionArn,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )
