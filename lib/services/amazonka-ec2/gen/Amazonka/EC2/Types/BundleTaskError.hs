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
-- Module      : Amazonka.EC2.Types.BundleTaskError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BundleTaskError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an error for BundleInstance.
--
-- /See:/ 'newBundleTaskError' smart constructor.
data BundleTaskError = BundleTaskError'
  { -- | The error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BundleTaskError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'bundleTaskError_code' - The error code.
--
-- 'message', 'bundleTaskError_message' - The error message.
newBundleTaskError ::
  BundleTaskError
newBundleTaskError =
  BundleTaskError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
bundleTaskError_code :: Lens.Lens' BundleTaskError (Prelude.Maybe Prelude.Text)
bundleTaskError_code = Lens.lens (\BundleTaskError' {code} -> code) (\s@BundleTaskError' {} a -> s {code = a} :: BundleTaskError)

-- | The error message.
bundleTaskError_message :: Lens.Lens' BundleTaskError (Prelude.Maybe Prelude.Text)
bundleTaskError_message = Lens.lens (\BundleTaskError' {message} -> message) (\s@BundleTaskError' {} a -> s {message = a} :: BundleTaskError)

instance Data.FromXML BundleTaskError where
  parseXML x =
    BundleTaskError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable BundleTaskError where
  hashWithSalt _salt BundleTaskError' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData BundleTaskError where
  rnf BundleTaskError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
