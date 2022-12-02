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
-- Module      : Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Information about the error for the burstable performance instance whose
-- credit option for CPU usage was not modified.
--
-- /See:/ 'newUnsuccessfulInstanceCreditSpecificationItemError' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItemError = UnsuccessfulInstanceCreditSpecificationItemError'
  { -- | The applicable error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe UnsuccessfulInstanceCreditSpecificationErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulInstanceCreditSpecificationItemError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'unsuccessfulInstanceCreditSpecificationItemError_message' - The applicable error message.
--
-- 'code', 'unsuccessfulInstanceCreditSpecificationItemError_code' - The error code.
newUnsuccessfulInstanceCreditSpecificationItemError ::
  UnsuccessfulInstanceCreditSpecificationItemError
newUnsuccessfulInstanceCreditSpecificationItemError =
  UnsuccessfulInstanceCreditSpecificationItemError'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The applicable error message.
unsuccessfulInstanceCreditSpecificationItemError_message :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItemError (Prelude.Maybe Prelude.Text)
unsuccessfulInstanceCreditSpecificationItemError_message = Lens.lens (\UnsuccessfulInstanceCreditSpecificationItemError' {message} -> message) (\s@UnsuccessfulInstanceCreditSpecificationItemError' {} a -> s {message = a} :: UnsuccessfulInstanceCreditSpecificationItemError)

-- | The error code.
unsuccessfulInstanceCreditSpecificationItemError_code :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItemError (Prelude.Maybe UnsuccessfulInstanceCreditSpecificationErrorCode)
unsuccessfulInstanceCreditSpecificationItemError_code = Lens.lens (\UnsuccessfulInstanceCreditSpecificationItemError' {code} -> code) (\s@UnsuccessfulInstanceCreditSpecificationItemError' {} a -> s {code = a} :: UnsuccessfulInstanceCreditSpecificationItemError)

instance
  Data.FromXML
    UnsuccessfulInstanceCreditSpecificationItemError
  where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItemError'
      Prelude.<$> (x Data..@? "message")
        Prelude.<*> (x Data..@? "code")

instance
  Prelude.Hashable
    UnsuccessfulInstanceCreditSpecificationItemError
  where
  hashWithSalt
    _salt
    UnsuccessfulInstanceCreditSpecificationItemError' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    UnsuccessfulInstanceCreditSpecificationItemError
  where
  rnf
    UnsuccessfulInstanceCreditSpecificationItemError' {..} =
      Prelude.rnf message `Prelude.seq` Prelude.rnf code
