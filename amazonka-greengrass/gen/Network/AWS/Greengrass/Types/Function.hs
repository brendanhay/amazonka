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

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.FunctionConfiguration
import qualified Network.AWS.Lens as Lens

-- | Information about a Lambda function.
--
-- /See:/ 'newFunction' smart constructor.
data Function = Function'
  { -- | The configuration of the Lambda function.
    functionConfiguration :: Core.Maybe FunctionConfiguration,
    -- | The ARN of the Lambda function.
    functionArn :: Core.Maybe Core.Text,
    -- | A descriptive or arbitrary ID for the function. This value must be
    -- unique within the function definition version. Max length is 128
    -- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  Function
newFunction pId_ =
  Function'
    { functionConfiguration = Core.Nothing,
      functionArn = Core.Nothing,
      id = pId_
    }

-- | The configuration of the Lambda function.
function_functionConfiguration :: Lens.Lens' Function (Core.Maybe FunctionConfiguration)
function_functionConfiguration = Lens.lens (\Function' {functionConfiguration} -> functionConfiguration) (\s@Function' {} a -> s {functionConfiguration = a} :: Function)

-- | The ARN of the Lambda function.
function_functionArn :: Lens.Lens' Function (Core.Maybe Core.Text)
function_functionArn = Lens.lens (\Function' {functionArn} -> functionArn) (\s@Function' {} a -> s {functionArn = a} :: Function)

-- | A descriptive or arbitrary ID for the function. This value must be
-- unique within the function definition version. Max length is 128
-- characters with pattern \'\'[a-zA-Z0-9:_-]+\'\'.
function_id :: Lens.Lens' Function Core.Text
function_id = Lens.lens (\Function' {id} -> id) (\s@Function' {} a -> s {id = a} :: Function)

instance Core.FromJSON Function where
  parseJSON =
    Core.withObject
      "Function"
      ( \x ->
          Function'
            Core.<$> (x Core..:? "FunctionConfiguration")
            Core.<*> (x Core..:? "FunctionArn")
            Core.<*> (x Core..: "Id")
      )

instance Core.Hashable Function

instance Core.NFData Function

instance Core.ToJSON Function where
  toJSON Function' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FunctionConfiguration" Core..=)
              Core.<$> functionConfiguration,
            ("FunctionArn" Core..=) Core.<$> functionArn,
            Core.Just ("Id" Core..= id)
          ]
      )
