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
-- Module      : Amazonka.EC2.Types.UnsuccessfulItemError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.UnsuccessfulItemError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the error that occurred. For more information about
-- errors, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error codes>.
--
-- /See:/ 'newUnsuccessfulItemError' smart constructor.
data UnsuccessfulItemError = UnsuccessfulItemError'
  { -- | The error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message accompanying the error code.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulItemError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'unsuccessfulItemError_code' - The error code.
--
-- 'message', 'unsuccessfulItemError_message' - The error message accompanying the error code.
newUnsuccessfulItemError ::
  UnsuccessfulItemError
newUnsuccessfulItemError =
  UnsuccessfulItemError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
unsuccessfulItemError_code :: Lens.Lens' UnsuccessfulItemError (Prelude.Maybe Prelude.Text)
unsuccessfulItemError_code = Lens.lens (\UnsuccessfulItemError' {code} -> code) (\s@UnsuccessfulItemError' {} a -> s {code = a} :: UnsuccessfulItemError)

-- | The error message accompanying the error code.
unsuccessfulItemError_message :: Lens.Lens' UnsuccessfulItemError (Prelude.Maybe Prelude.Text)
unsuccessfulItemError_message = Lens.lens (\UnsuccessfulItemError' {message} -> message) (\s@UnsuccessfulItemError' {} a -> s {message = a} :: UnsuccessfulItemError)

instance Data.FromXML UnsuccessfulItemError where
  parseXML x =
    UnsuccessfulItemError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable UnsuccessfulItemError where
  hashWithSalt _salt UnsuccessfulItemError' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData UnsuccessfulItemError where
  rnf UnsuccessfulItemError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
