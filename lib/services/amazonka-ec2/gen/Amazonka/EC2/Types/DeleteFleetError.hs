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
-- Module      : Amazonka.EC2.Types.DeleteFleetError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeleteFleetError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DeleteFleetErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Describes an EC2 Fleet error.
--
-- /See:/ 'newDeleteFleetError' smart constructor.
data DeleteFleetError = DeleteFleetError'
  { -- | The error code.
    code :: Prelude.Maybe DeleteFleetErrorCode,
    -- | The description for the error code.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'deleteFleetError_code' - The error code.
--
-- 'message', 'deleteFleetError_message' - The description for the error code.
newDeleteFleetError ::
  DeleteFleetError
newDeleteFleetError =
  DeleteFleetError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
deleteFleetError_code :: Lens.Lens' DeleteFleetError (Prelude.Maybe DeleteFleetErrorCode)
deleteFleetError_code = Lens.lens (\DeleteFleetError' {code} -> code) (\s@DeleteFleetError' {} a -> s {code = a} :: DeleteFleetError)

-- | The description for the error code.
deleteFleetError_message :: Lens.Lens' DeleteFleetError (Prelude.Maybe Prelude.Text)
deleteFleetError_message = Lens.lens (\DeleteFleetError' {message} -> message) (\s@DeleteFleetError' {} a -> s {message = a} :: DeleteFleetError)

instance Data.FromXML DeleteFleetError where
  parseXML x =
    DeleteFleetError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable DeleteFleetError where
  hashWithSalt _salt DeleteFleetError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData DeleteFleetError where
  rnf DeleteFleetError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
