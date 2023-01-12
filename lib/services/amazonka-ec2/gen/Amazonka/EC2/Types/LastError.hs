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
-- Module      : Amazonka.EC2.Types.LastError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LastError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The last error that occurred for a VPC endpoint.
--
-- /See:/ 'newLastError' smart constructor.
data LastError = LastError'
  { -- | The error code for the VPC endpoint error.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message for the VPC endpoint error.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'lastError_code' - The error code for the VPC endpoint error.
--
-- 'message', 'lastError_message' - The error message for the VPC endpoint error.
newLastError ::
  LastError
newLastError =
  LastError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code for the VPC endpoint error.
lastError_code :: Lens.Lens' LastError (Prelude.Maybe Prelude.Text)
lastError_code = Lens.lens (\LastError' {code} -> code) (\s@LastError' {} a -> s {code = a} :: LastError)

-- | The error message for the VPC endpoint error.
lastError_message :: Lens.Lens' LastError (Prelude.Maybe Prelude.Text)
lastError_message = Lens.lens (\LastError' {message} -> message) (\s@LastError' {} a -> s {message = a} :: LastError)

instance Data.FromXML LastError where
  parseXML x =
    LastError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable LastError where
  hashWithSalt _salt LastError' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData LastError where
  rnf LastError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
