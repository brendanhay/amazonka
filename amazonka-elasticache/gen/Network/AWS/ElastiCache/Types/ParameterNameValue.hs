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
-- Module      : Network.AWS.ElastiCache.Types.ParameterNameValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ParameterNameValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
--
-- /See:/ 'newParameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { -- | The value of the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | The name of the parameter.
    parameterName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterNameValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValue', 'parameterNameValue_parameterValue' - The value of the parameter.
--
-- 'parameterName', 'parameterNameValue_parameterName' - The name of the parameter.
newParameterNameValue ::
  ParameterNameValue
newParameterNameValue =
  ParameterNameValue'
    { parameterValue = Core.Nothing,
      parameterName = Core.Nothing
    }

-- | The value of the parameter.
parameterNameValue_parameterValue :: Lens.Lens' ParameterNameValue (Core.Maybe Core.Text)
parameterNameValue_parameterValue = Lens.lens (\ParameterNameValue' {parameterValue} -> parameterValue) (\s@ParameterNameValue' {} a -> s {parameterValue = a} :: ParameterNameValue)

-- | The name of the parameter.
parameterNameValue_parameterName :: Lens.Lens' ParameterNameValue (Core.Maybe Core.Text)
parameterNameValue_parameterName = Lens.lens (\ParameterNameValue' {parameterName} -> parameterName) (\s@ParameterNameValue' {} a -> s {parameterName = a} :: ParameterNameValue)

instance Core.Hashable ParameterNameValue

instance Core.NFData ParameterNameValue

instance Core.ToQuery ParameterNameValue where
  toQuery ParameterNameValue' {..} =
    Core.mconcat
      [ "ParameterValue" Core.=: parameterValue,
        "ParameterName" Core.=: parameterName
      ]
