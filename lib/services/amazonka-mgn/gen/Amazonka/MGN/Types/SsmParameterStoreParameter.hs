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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SsmParameterStoreParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.SsmParameterStoreParameterType
import qualified Amazonka.Prelude as Prelude

-- | Source server replication type.
--
-- /See:/ 'newSsmParameterStoreParameter' smart constructor.
data SsmParameterStoreParameter = SsmParameterStoreParameter'
  { -- | Source server replication type.
    parameterName :: Prelude.Text,
    -- | Source server replication type.
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
-- 'parameterName', 'ssmParameterStoreParameter_parameterName' - Source server replication type.
--
-- 'parameterType', 'ssmParameterStoreParameter_parameterType' - Source server replication type.
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

-- | Source server replication type.
ssmParameterStoreParameter_parameterName :: Lens.Lens' SsmParameterStoreParameter Prelude.Text
ssmParameterStoreParameter_parameterName = Lens.lens (\SsmParameterStoreParameter' {parameterName} -> parameterName) (\s@SsmParameterStoreParameter' {} a -> s {parameterName = a} :: SsmParameterStoreParameter)

-- | Source server replication type.
ssmParameterStoreParameter_parameterType :: Lens.Lens' SsmParameterStoreParameter SsmParameterStoreParameterType
ssmParameterStoreParameter_parameterType = Lens.lens (\SsmParameterStoreParameter' {parameterType} -> parameterType) (\s@SsmParameterStoreParameter' {} a -> s {parameterType = a} :: SsmParameterStoreParameter)

instance Core.FromJSON SsmParameterStoreParameter where
  parseJSON =
    Core.withObject
      "SsmParameterStoreParameter"
      ( \x ->
          SsmParameterStoreParameter'
            Prelude.<$> (x Core..: "parameterName")
            Prelude.<*> (x Core..: "parameterType")
      )

instance Prelude.Hashable SsmParameterStoreParameter where
  hashWithSalt _salt SsmParameterStoreParameter' {..} =
    _salt `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterType

instance Prelude.NFData SsmParameterStoreParameter where
  rnf SsmParameterStoreParameter' {..} =
    Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf parameterType

instance Core.ToJSON SsmParameterStoreParameter where
  toJSON SsmParameterStoreParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("parameterName" Core..= parameterName),
            Prelude.Just
              ("parameterType" Core..= parameterType)
          ]
      )
