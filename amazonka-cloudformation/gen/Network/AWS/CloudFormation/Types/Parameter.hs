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
-- Module      : Network.AWS.CloudFormation.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Parameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Parameter data type.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The input value associated with the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | During a stack update, use the existing parameter value that the stack
    -- is using for a given parameter key. If you specify @true@, do not
    -- specify a parameter value.
    usePreviousValue :: Prelude.Maybe Prelude.Bool,
    -- | The key associated with the parameter. If you don\'t specify a key and
    -- value for a particular parameter, AWS CloudFormation uses the default
    -- value that is specified in your template.
    parameterKey :: Prelude.Maybe Prelude.Text,
    -- | Read-only. The value that corresponds to a Systems Manager parameter
    -- key. This field is returned only for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types SSM parameter types>
    -- in the template.
    resolvedValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { parameterValue = Prelude.Nothing,
      usePreviousValue = Prelude.Nothing,
      parameterKey = Prelude.Nothing,
      resolvedValue = Prelude.Nothing
    }

-- | The input value associated with the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | During a stack update, use the existing parameter value that the stack
-- is using for a given parameter key. If you specify @true@, do not
-- specify a parameter value.
parameter_usePreviousValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Bool)
parameter_usePreviousValue = Lens.lens (\Parameter' {usePreviousValue} -> usePreviousValue) (\s@Parameter' {} a -> s {usePreviousValue = a} :: Parameter)

-- | The key associated with the parameter. If you don\'t specify a key and
-- value for a particular parameter, AWS CloudFormation uses the default
-- value that is specified in your template.
parameter_parameterKey :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterKey = Lens.lens (\Parameter' {parameterKey} -> parameterKey) (\s@Parameter' {} a -> s {parameterKey = a} :: Parameter)

-- | Read-only. The value that corresponds to a Systems Manager parameter
-- key. This field is returned only for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types SSM parameter types>
-- in the template.
parameter_resolvedValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_resolvedValue = Lens.lens (\Parameter' {resolvedValue} -> resolvedValue) (\s@Parameter' {} a -> s {resolvedValue = a} :: Parameter)

instance Prelude.FromXML Parameter where
  parseXML x =
    Parameter'
      Prelude.<$> (x Prelude..@? "ParameterValue")
      Prelude.<*> (x Prelude..@? "UsePreviousValue")
      Prelude.<*> (x Prelude..@? "ParameterKey")
      Prelude.<*> (x Prelude..@? "ResolvedValue")

instance Prelude.Hashable Parameter

instance Prelude.NFData Parameter

instance Prelude.ToQuery Parameter where
  toQuery Parameter' {..} =
    Prelude.mconcat
      [ "ParameterValue" Prelude.=: parameterValue,
        "UsePreviousValue" Prelude.=: usePreviousValue,
        "ParameterKey" Prelude.=: parameterKey,
        "ResolvedValue" Prelude.=: resolvedValue
      ]
