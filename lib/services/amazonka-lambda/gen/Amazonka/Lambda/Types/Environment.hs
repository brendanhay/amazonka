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
-- Module      : Amazonka.Lambda.Types.Environment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.Environment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A function\'s environment variable settings. You can use environment
-- variables to adjust your function\'s behavior without updating code. An
-- environment variable is a pair of strings that are stored in a
-- function\'s version-specific configuration.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | Environment variable key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html Using Lambda environment variables>.
    variables :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variables', 'environment_variables' - Environment variable key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html Using Lambda environment variables>.
newEnvironment ::
  Environment
newEnvironment =
  Environment' {variables = Prelude.Nothing}

-- | Environment variable key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html Using Lambda environment variables>.
environment_variables :: Lens.Lens' Environment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
environment_variables = Lens.lens (\Environment' {variables} -> variables) (\s@Environment' {} a -> s {variables = a} :: Environment) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Prelude.Hashable Environment where
  hashWithSalt _salt Environment' {..} =
    _salt `Prelude.hashWithSalt` variables

instance Prelude.NFData Environment where
  rnf Environment' {..} = Prelude.rnf variables

instance Data.ToJSON Environment where
  toJSON Environment' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Variables" Data..=) Prelude.<$> variables]
      )
