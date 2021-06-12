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
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ParameterValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Parameter value of the application.
--
-- /See:/ 'newParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { -- | The input value associated with the parameter.
    value :: Core.Text,
    -- | The key associated with the parameter. If you don\'t specify a key and
    -- value for a particular parameter, AWS CloudFormation uses the default
    -- value that is specified in your template.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'parameterValue_value' - The input value associated with the parameter.
--
-- 'name', 'parameterValue_name' - The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
newParameterValue ::
  -- | 'value'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  ParameterValue
newParameterValue pValue_ pName_ =
  ParameterValue' {value = pValue_, name = pName_}

-- | The input value associated with the parameter.
parameterValue_value :: Lens.Lens' ParameterValue Core.Text
parameterValue_value = Lens.lens (\ParameterValue' {value} -> value) (\s@ParameterValue' {} a -> s {value = a} :: ParameterValue)

-- | The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
parameterValue_name :: Lens.Lens' ParameterValue Core.Text
parameterValue_name = Lens.lens (\ParameterValue' {name} -> name) (\s@ParameterValue' {} a -> s {name = a} :: ParameterValue)

instance Core.Hashable ParameterValue

instance Core.NFData ParameterValue

instance Core.ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("value" Core..= value),
            Core.Just ("name" Core..= name)
          ]
      )
