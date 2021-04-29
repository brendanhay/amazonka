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
-- Module      : Network.AWS.EC2.Types.EnclaveOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnclaveOptionsRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
-- in the /AWS Nitro Enclaves User Guide/.
--
-- /See:/ 'newEnclaveOptionsRequest' smart constructor.
data EnclaveOptionsRequest = EnclaveOptionsRequest'
  { -- | To enable the instance for AWS Nitro Enclaves, set this parameter to
    -- @true@.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnclaveOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'enclaveOptionsRequest_enabled' - To enable the instance for AWS Nitro Enclaves, set this parameter to
-- @true@.
newEnclaveOptionsRequest ::
  EnclaveOptionsRequest
newEnclaveOptionsRequest =
  EnclaveOptionsRequest' {enabled = Prelude.Nothing}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to
-- @true@.
enclaveOptionsRequest_enabled :: Lens.Lens' EnclaveOptionsRequest (Prelude.Maybe Prelude.Bool)
enclaveOptionsRequest_enabled = Lens.lens (\EnclaveOptionsRequest' {enabled} -> enabled) (\s@EnclaveOptionsRequest' {} a -> s {enabled = a} :: EnclaveOptionsRequest)

instance Prelude.Hashable EnclaveOptionsRequest

instance Prelude.NFData EnclaveOptionsRequest

instance Prelude.ToQuery EnclaveOptionsRequest where
  toQuery EnclaveOptionsRequest' {..} =
    Prelude.mconcat ["Enabled" Prelude.=: enabled]
