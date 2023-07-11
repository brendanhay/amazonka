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
-- Module      : Amazonka.MGN.Types.SsmParameterStoreParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SsmParameterStoreParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.SsmParameterStoreParameterType
import qualified Amazonka.Prelude as Prelude

-- | AWS Systems Manager Parameter Store parameter.
--
-- /See:/ 'newSsmParameterStoreParameter' smart constructor.
data SsmParameterStoreParameter = SsmParameterStoreParameter'
  { -- | AWS Systems Manager Parameter Store parameter name.
    parameterName :: Prelude.Text,
    -- | AWS Systems Manager Parameter Store parameter type.
    parameterType :: SsmParameterStoreParameterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SsmParameterStoreParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterName', 'ssmParameterStoreParameter_parameterName' - AWS Systems Manager Parameter Store parameter name.
--
-- 'parameterType', 'ssmParameterStoreParameter_parameterType' - AWS Systems Manager Parameter Store parameter type.
newSsmParameterStoreParameter ::
  -- | 'parameterName'
  Prelude.Text ->
  -- | 'parameterType'
  SsmParameterStoreParameterType ->
  SsmParameterStoreParameter
newSsmParameterStoreParameter
  pParameterName_
  pParameterType_ =
    SsmParameterStoreParameter'
      { parameterName =
          pParameterName_,
        parameterType = pParameterType_
      }

-- | AWS Systems Manager Parameter Store parameter name.
ssmParameterStoreParameter_parameterName :: Lens.Lens' SsmParameterStoreParameter Prelude.Text
ssmParameterStoreParameter_parameterName = Lens.lens (\SsmParameterStoreParameter' {parameterName} -> parameterName) (\s@SsmParameterStoreParameter' {} a -> s {parameterName = a} :: SsmParameterStoreParameter)

-- | AWS Systems Manager Parameter Store parameter type.
ssmParameterStoreParameter_parameterType :: Lens.Lens' SsmParameterStoreParameter SsmParameterStoreParameterType
ssmParameterStoreParameter_parameterType = Lens.lens (\SsmParameterStoreParameter' {parameterType} -> parameterType) (\s@SsmParameterStoreParameter' {} a -> s {parameterType = a} :: SsmParameterStoreParameter)

instance Data.FromJSON SsmParameterStoreParameter where
  parseJSON =
    Data.withObject
      "SsmParameterStoreParameter"
      ( \x ->
          SsmParameterStoreParameter'
            Prelude.<$> (x Data..: "parameterName")
            Prelude.<*> (x Data..: "parameterType")
      )

instance Prelude.Hashable SsmParameterStoreParameter where
  hashWithSalt _salt SsmParameterStoreParameter' {..} =
    _salt
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterType

instance Prelude.NFData SsmParameterStoreParameter where
  rnf SsmParameterStoreParameter' {..} =
    Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf parameterType

instance Data.ToJSON SsmParameterStoreParameter where
  toJSON SsmParameterStoreParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("parameterName" Data..= parameterName),
            Prelude.Just
              ("parameterType" Data..= parameterType)
          ]
      )
