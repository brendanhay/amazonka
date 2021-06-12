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
-- Module      : Network.AWS.CloudFormation.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Parameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Parameter data type.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The input value associated with the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | During a stack update, use the existing parameter value that the stack
    -- is using for a given parameter key. If you specify @true@, do not
    -- specify a parameter value.
    usePreviousValue :: Core.Maybe Core.Bool,
    -- | The key associated with the parameter. If you don\'t specify a key and
    -- value for a particular parameter, AWS CloudFormation uses the default
    -- value that is specified in your template.
    parameterKey :: Core.Maybe Core.Text,
    -- | Read-only. The value that corresponds to a Systems Manager parameter
    -- key. This field is returned only for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types SSM parameter types>
    -- in the template.
    resolvedValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValue', 'parameter_parameterValue' - The input value associated with the parameter.
--
-- 'usePreviousValue', 'parameter_usePreviousValue' - During a stack update, use the existing parameter value that the stack
-- is using for a given parameter key. If you specify @true@, do not
-- specify a parameter value.
--
-- 'parameterKey', 'parameter_parameterKey' - The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
--
-- 'resolvedValue', 'parameter_resolvedValue' - Read-only. The value that corresponds to a Systems Manager parameter
-- key. This field is returned only for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types SSM parameter types>
-- in the template.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { parameterValue = Core.Nothing,
      usePreviousValue = Core.Nothing,
      parameterKey = Core.Nothing,
      resolvedValue = Core.Nothing
    }

-- | The input value associated with the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | During a stack update, use the existing parameter value that the stack
-- is using for a given parameter key. If you specify @true@, do not
-- specify a parameter value.
parameter_usePreviousValue :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
parameter_usePreviousValue = Lens.lens (\Parameter' {usePreviousValue} -> usePreviousValue) (\s@Parameter' {} a -> s {usePreviousValue = a} :: Parameter)

-- | The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
parameter_parameterKey :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterKey = Lens.lens (\Parameter' {parameterKey} -> parameterKey) (\s@Parameter' {} a -> s {parameterKey = a} :: Parameter)

-- | Read-only. The value that corresponds to a Systems Manager parameter
-- key. This field is returned only for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types SSM parameter types>
-- in the template.
parameter_resolvedValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_resolvedValue = Lens.lens (\Parameter' {resolvedValue} -> resolvedValue) (\s@Parameter' {} a -> s {resolvedValue = a} :: Parameter)

instance Core.FromXML Parameter where
  parseXML x =
    Parameter'
      Core.<$> (x Core..@? "ParameterValue")
      Core.<*> (x Core..@? "UsePreviousValue")
      Core.<*> (x Core..@? "ParameterKey")
      Core.<*> (x Core..@? "ResolvedValue")

instance Core.Hashable Parameter

instance Core.NFData Parameter

instance Core.ToQuery Parameter where
  toQuery Parameter' {..} =
    Core.mconcat
      [ "ParameterValue" Core.=: parameterValue,
        "UsePreviousValue" Core.=: usePreviousValue,
        "ParameterKey" Core.=: parameterKey,
        "ResolvedValue" Core.=: resolvedValue
      ]
