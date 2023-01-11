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
-- Module      : Amazonka.Config.Types.ConformancePackInputParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackInputParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input parameters in the form of key-value pairs for the conformance
-- pack, both of which you define. Keys can have a maximum character length
-- of 255 characters, and values can have a maximum length of 4096
-- characters.
--
-- /See:/ 'newConformancePackInputParameter' smart constructor.
data ConformancePackInputParameter = ConformancePackInputParameter'
  { -- | One part of a key-value pair.
    parameterName :: Prelude.Text,
    -- | Another part of the key-value pair.
    parameterValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackInputParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterName', 'conformancePackInputParameter_parameterName' - One part of a key-value pair.
--
-- 'parameterValue', 'conformancePackInputParameter_parameterValue' - Another part of the key-value pair.
newConformancePackInputParameter ::
  -- | 'parameterName'
  Prelude.Text ->
  -- | 'parameterValue'
  Prelude.Text ->
  ConformancePackInputParameter
newConformancePackInputParameter
  pParameterName_
  pParameterValue_ =
    ConformancePackInputParameter'
      { parameterName =
          pParameterName_,
        parameterValue = pParameterValue_
      }

-- | One part of a key-value pair.
conformancePackInputParameter_parameterName :: Lens.Lens' ConformancePackInputParameter Prelude.Text
conformancePackInputParameter_parameterName = Lens.lens (\ConformancePackInputParameter' {parameterName} -> parameterName) (\s@ConformancePackInputParameter' {} a -> s {parameterName = a} :: ConformancePackInputParameter)

-- | Another part of the key-value pair.
conformancePackInputParameter_parameterValue :: Lens.Lens' ConformancePackInputParameter Prelude.Text
conformancePackInputParameter_parameterValue = Lens.lens (\ConformancePackInputParameter' {parameterValue} -> parameterValue) (\s@ConformancePackInputParameter' {} a -> s {parameterValue = a} :: ConformancePackInputParameter)

instance Data.FromJSON ConformancePackInputParameter where
  parseJSON =
    Data.withObject
      "ConformancePackInputParameter"
      ( \x ->
          ConformancePackInputParameter'
            Prelude.<$> (x Data..: "ParameterName")
            Prelude.<*> (x Data..: "ParameterValue")
      )

instance
  Prelude.Hashable
    ConformancePackInputParameter
  where
  hashWithSalt _salt ConformancePackInputParameter' {..} =
    _salt `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterValue

instance Prelude.NFData ConformancePackInputParameter where
  rnf ConformancePackInputParameter' {..} =
    Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf parameterValue

instance Data.ToJSON ConformancePackInputParameter where
  toJSON ConformancePackInputParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParameterName" Data..= parameterName),
            Prelude.Just
              ("ParameterValue" Data..= parameterValue)
          ]
      )
