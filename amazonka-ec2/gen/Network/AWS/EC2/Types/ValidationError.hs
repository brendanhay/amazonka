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
-- Module      : Network.AWS.EC2.Types.ValidationError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ValidationError where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The error code and error message that is returned for a parameter or
-- parameter combination that is not valid when a new launch template or
-- new version of a launch template is created.
--
-- /See:/ 'newValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The error message that describes why the parameter or parameter
    -- combination is not valid. For more information about error messages, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code that indicates why the parameter or parameter combination
    -- is not valid. For more information about error codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ValidationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'validationError_message' - The error message that describes why the parameter or parameter
-- combination is not valid. For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
--
-- 'code', 'validationError_code' - The error code that indicates why the parameter or parameter combination
-- is not valid. For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
newValidationError ::
  ValidationError
newValidationError =
  ValidationError'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The error message that describes why the parameter or parameter
-- combination is not valid. For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
validationError_message :: Lens.Lens' ValidationError (Prelude.Maybe Prelude.Text)
validationError_message = Lens.lens (\ValidationError' {message} -> message) (\s@ValidationError' {} a -> s {message = a} :: ValidationError)

-- | The error code that indicates why the parameter or parameter combination
-- is not valid. For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
validationError_code :: Lens.Lens' ValidationError (Prelude.Maybe Prelude.Text)
validationError_code = Lens.lens (\ValidationError' {code} -> code) (\s@ValidationError' {} a -> s {code = a} :: ValidationError)

instance Prelude.FromXML ValidationError where
  parseXML x =
    ValidationError'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable ValidationError

instance Prelude.NFData ValidationError
