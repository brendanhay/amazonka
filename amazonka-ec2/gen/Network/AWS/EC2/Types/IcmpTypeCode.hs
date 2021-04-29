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
-- Module      : Network.AWS.EC2.Types.IcmpTypeCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IcmpTypeCode where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the ICMP type and code.
--
-- /See:/ 'newIcmpTypeCode' smart constructor.
data IcmpTypeCode = IcmpTypeCode'
  { -- | The ICMP code. A value of -1 means all codes for the specified ICMP
    -- type.
    code :: Prelude.Maybe Prelude.Int,
    -- | The ICMP type. A value of -1 means all types.
    type' :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IcmpTypeCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'icmpTypeCode_code' - The ICMP code. A value of -1 means all codes for the specified ICMP
-- type.
--
-- 'type'', 'icmpTypeCode_type' - The ICMP type. A value of -1 means all types.
newIcmpTypeCode ::
  IcmpTypeCode
newIcmpTypeCode =
  IcmpTypeCode'
    { code = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ICMP code. A value of -1 means all codes for the specified ICMP
-- type.
icmpTypeCode_code :: Lens.Lens' IcmpTypeCode (Prelude.Maybe Prelude.Int)
icmpTypeCode_code = Lens.lens (\IcmpTypeCode' {code} -> code) (\s@IcmpTypeCode' {} a -> s {code = a} :: IcmpTypeCode)

-- | The ICMP type. A value of -1 means all types.
icmpTypeCode_type :: Lens.Lens' IcmpTypeCode (Prelude.Maybe Prelude.Int)
icmpTypeCode_type = Lens.lens (\IcmpTypeCode' {type'} -> type') (\s@IcmpTypeCode' {} a -> s {type' = a} :: IcmpTypeCode)

instance Prelude.FromXML IcmpTypeCode where
  parseXML x =
    IcmpTypeCode'
      Prelude.<$> (x Prelude..@? "code")
      Prelude.<*> (x Prelude..@? "type")

instance Prelude.Hashable IcmpTypeCode

instance Prelude.NFData IcmpTypeCode

instance Prelude.ToQuery IcmpTypeCode where
  toQuery IcmpTypeCode' {..} =
    Prelude.mconcat
      ["Code" Prelude.=: code, "Type" Prelude.=: type']
