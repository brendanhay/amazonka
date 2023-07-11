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
-- Module      : Amazonka.CloudFormation.Types.ParameterConstraints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ParameterConstraints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of criteria that CloudFormation uses to validate parameter values.
-- Although other constraints might be defined in the stack template,
-- CloudFormation returns only the @AllowedValues@ property.
--
-- /See:/ 'newParameterConstraints' smart constructor.
data ParameterConstraints = ParameterConstraints'
  { -- | A list of values that are permitted for a parameter.
    allowedValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'parameterConstraints_allowedValues' - A list of values that are permitted for a parameter.
newParameterConstraints ::
  ParameterConstraints
newParameterConstraints =
  ParameterConstraints'
    { allowedValues =
        Prelude.Nothing
    }

-- | A list of values that are permitted for a parameter.
parameterConstraints_allowedValues :: Lens.Lens' ParameterConstraints (Prelude.Maybe [Prelude.Text])
parameterConstraints_allowedValues = Lens.lens (\ParameterConstraints' {allowedValues} -> allowedValues) (\s@ParameterConstraints' {} a -> s {allowedValues = a} :: ParameterConstraints) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ParameterConstraints where
  parseXML x =
    ParameterConstraints'
      Prelude.<$> ( x
                      Data..@? "AllowedValues"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable ParameterConstraints where
  hashWithSalt _salt ParameterConstraints' {..} =
    _salt `Prelude.hashWithSalt` allowedValues

instance Prelude.NFData ParameterConstraints where
  rnf ParameterConstraints' {..} =
    Prelude.rnf allowedValues
