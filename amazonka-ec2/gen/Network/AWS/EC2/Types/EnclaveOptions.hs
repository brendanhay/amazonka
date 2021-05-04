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
-- Module      : Network.AWS.EC2.Types.EnclaveOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnclaveOptions where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /See:/ 'newEnclaveOptions' smart constructor.
data EnclaveOptions = EnclaveOptions'
  { -- | If this parameter is set to @true@, the instance is enabled for AWS
    -- Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnclaveOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'enclaveOptions_enabled' - If this parameter is set to @true@, the instance is enabled for AWS
-- Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
newEnclaveOptions ::
  EnclaveOptions
newEnclaveOptions =
  EnclaveOptions' {enabled = Prelude.Nothing}

-- | If this parameter is set to @true@, the instance is enabled for AWS
-- Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
enclaveOptions_enabled :: Lens.Lens' EnclaveOptions (Prelude.Maybe Prelude.Bool)
enclaveOptions_enabled = Lens.lens (\EnclaveOptions' {enabled} -> enabled) (\s@EnclaveOptions' {} a -> s {enabled = a} :: EnclaveOptions)

instance Prelude.FromXML EnclaveOptions where
  parseXML x =
    EnclaveOptions'
      Prelude.<$> (x Prelude..@? "enabled")

instance Prelude.Hashable EnclaveOptions

instance Prelude.NFData EnclaveOptions
