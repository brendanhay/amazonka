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
-- Module      : Amazonka.EC2.Types.IKEVersionsListValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IKEVersionsListValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The internet key exchange (IKE) version permitted for the VPN tunnel.
--
-- /See:/ 'newIKEVersionsListValue' smart constructor.
data IKEVersionsListValue = IKEVersionsListValue'
  { -- | The IKE version.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IKEVersionsListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'iKEVersionsListValue_value' - The IKE version.
newIKEVersionsListValue ::
  IKEVersionsListValue
newIKEVersionsListValue =
  IKEVersionsListValue' {value = Prelude.Nothing}

-- | The IKE version.
iKEVersionsListValue_value :: Lens.Lens' IKEVersionsListValue (Prelude.Maybe Prelude.Text)
iKEVersionsListValue_value = Lens.lens (\IKEVersionsListValue' {value} -> value) (\s@IKEVersionsListValue' {} a -> s {value = a} :: IKEVersionsListValue)

instance Data.FromXML IKEVersionsListValue where
  parseXML x =
    IKEVersionsListValue'
      Prelude.<$> (x Data..@? "value")

instance Prelude.Hashable IKEVersionsListValue where
  hashWithSalt _salt IKEVersionsListValue' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData IKEVersionsListValue where
  rnf IKEVersionsListValue' {..} = Prelude.rnf value
