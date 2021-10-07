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
-- Module      : Network.AWS.Lambda.Types.Environment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Environment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A function\'s environment variable settings. You can use environment
-- variables to adjust your function\'s behavior without updating code. An
-- environment variable is a pair of strings that are stored in a
-- function\'s version-specific configuration.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | Environment variable key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html Using Lambda environment variables>.
    variables :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)))
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
environment_variables = Lens.lens (\Environment' {variables} -> variables) (\s@Environment' {} a -> s {variables = a} :: Environment) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

instance Prelude.Hashable Environment

instance Prelude.NFData Environment

instance Core.ToJSON Environment where
  toJSON Environment' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Variables" Core..=) Prelude.<$> variables]
      )
