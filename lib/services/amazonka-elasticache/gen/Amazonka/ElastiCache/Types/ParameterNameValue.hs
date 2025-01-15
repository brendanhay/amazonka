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
-- Module      : Amazonka.ElastiCache.Types.ParameterNameValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ParameterNameValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
--
-- /See:/ 'newParameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterNameValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterName', 'parameterNameValue_parameterName' - The name of the parameter.
--
-- 'parameterValue', 'parameterNameValue_parameterValue' - The value of the parameter.
newParameterNameValue ::
  ParameterNameValue
newParameterNameValue =
  ParameterNameValue'
    { parameterName =
        Prelude.Nothing,
      parameterValue = Prelude.Nothing
    }

-- | The name of the parameter.
parameterNameValue_parameterName :: Lens.Lens' ParameterNameValue (Prelude.Maybe Prelude.Text)
parameterNameValue_parameterName = Lens.lens (\ParameterNameValue' {parameterName} -> parameterName) (\s@ParameterNameValue' {} a -> s {parameterName = a} :: ParameterNameValue)

-- | The value of the parameter.
parameterNameValue_parameterValue :: Lens.Lens' ParameterNameValue (Prelude.Maybe Prelude.Text)
parameterNameValue_parameterValue = Lens.lens (\ParameterNameValue' {parameterValue} -> parameterValue) (\s@ParameterNameValue' {} a -> s {parameterValue = a} :: ParameterNameValue)

instance Prelude.Hashable ParameterNameValue where
  hashWithSalt _salt ParameterNameValue' {..} =
    _salt
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterValue

instance Prelude.NFData ParameterNameValue where
  rnf ParameterNameValue' {..} =
    Prelude.rnf parameterName `Prelude.seq`
      Prelude.rnf parameterValue

instance Data.ToQuery ParameterNameValue where
  toQuery ParameterNameValue' {..} =
    Prelude.mconcat
      [ "ParameterName" Data.=: parameterName,
        "ParameterValue" Data.=: parameterValue
      ]
