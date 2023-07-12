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
-- Module      : Amazonka.EC2.Types.ValidationError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ValidationError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The error code and error message that is returned for a parameter or
-- parameter combination that is not valid when a new launch template or
-- new version of a launch template is created.
--
-- /See:/ 'newValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The error code that indicates why the parameter or parameter combination
    -- is not valid. For more information about error codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message that describes why the parameter or parameter
    -- combination is not valid. For more information about error messages, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'validationError_code' - The error code that indicates why the parameter or parameter combination
-- is not valid. For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
--
-- 'message', 'validationError_message' - The error message that describes why the parameter or parameter
-- combination is not valid. For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
newValidationError ::
  ValidationError
newValidationError =
  ValidationError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code that indicates why the parameter or parameter combination
-- is not valid. For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
validationError_code :: Lens.Lens' ValidationError (Prelude.Maybe Prelude.Text)
validationError_code = Lens.lens (\ValidationError' {code} -> code) (\s@ValidationError' {} a -> s {code = a} :: ValidationError)

-- | The error message that describes why the parameter or parameter
-- combination is not valid. For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
validationError_message :: Lens.Lens' ValidationError (Prelude.Maybe Prelude.Text)
validationError_message = Lens.lens (\ValidationError' {message} -> message) (\s@ValidationError' {} a -> s {message = a} :: ValidationError)

instance Data.FromXML ValidationError where
  parseXML x =
    ValidationError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable ValidationError where
  hashWithSalt _salt ValidationError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ValidationError where
  rnf ValidationError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
