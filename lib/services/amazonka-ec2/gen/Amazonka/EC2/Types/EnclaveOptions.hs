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
-- Module      : Amazonka.EC2.Types.EnclaveOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnclaveOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves.
--
-- /See:/ 'newEnclaveOptions' smart constructor.
data EnclaveOptions = EnclaveOptions'
  { -- | If this parameter is set to @true@, the instance is enabled for Amazon
    -- Web Services Nitro Enclaves; otherwise, it is not enabled for Amazon Web
    -- Services Nitro Enclaves.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnclaveOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'enclaveOptions_enabled' - If this parameter is set to @true@, the instance is enabled for Amazon
-- Web Services Nitro Enclaves; otherwise, it is not enabled for Amazon Web
-- Services Nitro Enclaves.
newEnclaveOptions ::
  EnclaveOptions
newEnclaveOptions =
  EnclaveOptions' {enabled = Prelude.Nothing}

-- | If this parameter is set to @true@, the instance is enabled for Amazon
-- Web Services Nitro Enclaves; otherwise, it is not enabled for Amazon Web
-- Services Nitro Enclaves.
enclaveOptions_enabled :: Lens.Lens' EnclaveOptions (Prelude.Maybe Prelude.Bool)
enclaveOptions_enabled = Lens.lens (\EnclaveOptions' {enabled} -> enabled) (\s@EnclaveOptions' {} a -> s {enabled = a} :: EnclaveOptions)

instance Data.FromXML EnclaveOptions where
  parseXML x =
    EnclaveOptions' Prelude.<$> (x Data..@? "enabled")

instance Prelude.Hashable EnclaveOptions where
  hashWithSalt _salt EnclaveOptions' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData EnclaveOptions where
  rnf EnclaveOptions' {..} = Prelude.rnf enabled
