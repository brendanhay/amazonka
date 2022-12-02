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
-- Module      : Amazonka.SecurityHub.Types.IcmpTypeCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.IcmpTypeCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Internet Control Message Protocol (ICMP) type and code.
--
-- /See:/ 'newIcmpTypeCode' smart constructor.
data IcmpTypeCode = IcmpTypeCode'
  { -- | The ICMP type for which to deny or allow access. To deny or allow all
    -- types, use the value @-1@.
    type' :: Prelude.Maybe Prelude.Int,
    -- | The ICMP code for which to deny or allow access. To deny or allow all
    -- codes, use the value @-1@.
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
-- 'type'', 'icmpTypeCode_type' - The ICMP type for which to deny or allow access. To deny or allow all
-- types, use the value @-1@.
--
-- 'code', 'icmpTypeCode_code' - The ICMP code for which to deny or allow access. To deny or allow all
-- codes, use the value @-1@.
newIcmpTypeCode ::
  IcmpTypeCode
newIcmpTypeCode =
  IcmpTypeCode'
    { type' = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The ICMP type for which to deny or allow access. To deny or allow all
-- types, use the value @-1@.
icmpTypeCode_type :: Lens.Lens' IcmpTypeCode (Prelude.Maybe Prelude.Int)
icmpTypeCode_type = Lens.lens (\IcmpTypeCode' {type'} -> type') (\s@IcmpTypeCode' {} a -> s {type' = a} :: IcmpTypeCode)

-- | The ICMP code for which to deny or allow access. To deny or allow all
-- codes, use the value @-1@.
icmpTypeCode_code :: Lens.Lens' IcmpTypeCode (Prelude.Maybe Prelude.Int)
icmpTypeCode_code = Lens.lens (\IcmpTypeCode' {code} -> code) (\s@IcmpTypeCode' {} a -> s {code = a} :: IcmpTypeCode)

instance Data.FromJSON IcmpTypeCode where
  parseJSON =
    Data.withObject
      "IcmpTypeCode"
      ( \x ->
          IcmpTypeCode'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Code")
      )

instance Prelude.Hashable IcmpTypeCode where
  hashWithSalt _salt IcmpTypeCode' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` code

instance Prelude.NFData IcmpTypeCode where
  rnf IcmpTypeCode' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf code

instance Data.ToJSON IcmpTypeCode where
  toJSON IcmpTypeCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("Code" Data..=) Prelude.<$> code
          ]
      )
