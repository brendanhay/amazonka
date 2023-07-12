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
-- Module      : Amazonka.EC2.Types.EnclaveOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnclaveOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves. For more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is Amazon Web Services Nitro Enclaves?>
-- in the /Amazon Web Services Nitro Enclaves User Guide/.
--
-- /See:/ 'newEnclaveOptionsRequest' smart constructor.
data EnclaveOptionsRequest = EnclaveOptionsRequest'
  { -- | To enable the instance for Amazon Web Services Nitro Enclaves, set this
    -- parameter to @true@.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnclaveOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'enclaveOptionsRequest_enabled' - To enable the instance for Amazon Web Services Nitro Enclaves, set this
-- parameter to @true@.
newEnclaveOptionsRequest ::
  EnclaveOptionsRequest
newEnclaveOptionsRequest =
  EnclaveOptionsRequest' {enabled = Prelude.Nothing}

-- | To enable the instance for Amazon Web Services Nitro Enclaves, set this
-- parameter to @true@.
enclaveOptionsRequest_enabled :: Lens.Lens' EnclaveOptionsRequest (Prelude.Maybe Prelude.Bool)
enclaveOptionsRequest_enabled = Lens.lens (\EnclaveOptionsRequest' {enabled} -> enabled) (\s@EnclaveOptionsRequest' {} a -> s {enabled = a} :: EnclaveOptionsRequest)

instance Prelude.Hashable EnclaveOptionsRequest where
  hashWithSalt _salt EnclaveOptionsRequest' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData EnclaveOptionsRequest where
  rnf EnclaveOptionsRequest' {..} = Prelude.rnf enabled

instance Data.ToQuery EnclaveOptionsRequest where
  toQuery EnclaveOptionsRequest' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
