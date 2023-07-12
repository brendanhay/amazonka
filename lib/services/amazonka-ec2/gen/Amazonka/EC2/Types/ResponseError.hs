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
-- Module      : Amazonka.EC2.Types.ResponseError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ResponseError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the error that\'s returned when you cannot delete a launch
-- template version.
--
-- /See:/ 'newResponseError' smart constructor.
data ResponseError = ResponseError'
  { -- | The error code.
    code :: Prelude.Maybe LaunchTemplateErrorCode,
    -- | The error message, if applicable.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'responseError_code' - The error code.
--
-- 'message', 'responseError_message' - The error message, if applicable.
newResponseError ::
  ResponseError
newResponseError =
  ResponseError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
responseError_code :: Lens.Lens' ResponseError (Prelude.Maybe LaunchTemplateErrorCode)
responseError_code = Lens.lens (\ResponseError' {code} -> code) (\s@ResponseError' {} a -> s {code = a} :: ResponseError)

-- | The error message, if applicable.
responseError_message :: Lens.Lens' ResponseError (Prelude.Maybe Prelude.Text)
responseError_message = Lens.lens (\ResponseError' {message} -> message) (\s@ResponseError' {} a -> s {message = a} :: ResponseError)

instance Data.FromXML ResponseError where
  parseXML x =
    ResponseError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable ResponseError where
  hashWithSalt _salt ResponseError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ResponseError where
  rnf ResponseError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
