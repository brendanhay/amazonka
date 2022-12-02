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
-- Module      : Amazonka.EC2.Types.IcmpTypeCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IcmpTypeCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the ICMP type and code.
--
-- /See:/ 'newIcmpTypeCode' smart constructor.
data IcmpTypeCode = IcmpTypeCode'
  { -- | The ICMP type. A value of -1 means all types.
    type' :: Prelude.Maybe Prelude.Int,
    -- | The ICMP code. A value of -1 means all codes for the specified ICMP
    -- type.
    code :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IcmpTypeCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'icmpTypeCode_type' - The ICMP type. A value of -1 means all types.
--
-- 'code', 'icmpTypeCode_code' - The ICMP code. A value of -1 means all codes for the specified ICMP
-- type.
newIcmpTypeCode ::
  IcmpTypeCode
newIcmpTypeCode =
  IcmpTypeCode'
    { type' = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The ICMP type. A value of -1 means all types.
icmpTypeCode_type :: Lens.Lens' IcmpTypeCode (Prelude.Maybe Prelude.Int)
icmpTypeCode_type = Lens.lens (\IcmpTypeCode' {type'} -> type') (\s@IcmpTypeCode' {} a -> s {type' = a} :: IcmpTypeCode)

-- | The ICMP code. A value of -1 means all codes for the specified ICMP
-- type.
icmpTypeCode_code :: Lens.Lens' IcmpTypeCode (Prelude.Maybe Prelude.Int)
icmpTypeCode_code = Lens.lens (\IcmpTypeCode' {code} -> code) (\s@IcmpTypeCode' {} a -> s {code = a} :: IcmpTypeCode)

instance Data.FromXML IcmpTypeCode where
  parseXML x =
    IcmpTypeCode'
      Prelude.<$> (x Data..@? "type") Prelude.<*> (x Data..@? "code")

instance Prelude.Hashable IcmpTypeCode where
  hashWithSalt _salt IcmpTypeCode' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` code

instance Prelude.NFData IcmpTypeCode where
  rnf IcmpTypeCode' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf code

instance Data.ToQuery IcmpTypeCode where
  toQuery IcmpTypeCode' {..} =
    Prelude.mconcat
      ["Type" Data.=: type', "Code" Data.=: code]
