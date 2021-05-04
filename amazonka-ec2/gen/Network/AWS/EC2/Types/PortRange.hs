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
-- Module      : Network.AWS.EC2.Types.PortRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PortRange where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a range of ports.
--
-- /See:/ 'newPortRange' smart constructor.
data PortRange = PortRange'
  { -- | The last port in the range.
    to :: Prelude.Maybe Prelude.Int,
    -- | The first port in the range.
    from :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PortRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'to', 'portRange_to' - The last port in the range.
--
-- 'from', 'portRange_from' - The first port in the range.
newPortRange ::
  PortRange
newPortRange =
  PortRange'
    { to = Prelude.Nothing,
      from = Prelude.Nothing
    }

-- | The last port in the range.
portRange_to :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_to = Lens.lens (\PortRange' {to} -> to) (\s@PortRange' {} a -> s {to = a} :: PortRange)

-- | The first port in the range.
portRange_from :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_from = Lens.lens (\PortRange' {from} -> from) (\s@PortRange' {} a -> s {from = a} :: PortRange)

instance Prelude.FromXML PortRange where
  parseXML x =
    PortRange'
      Prelude.<$> (x Prelude..@? "to")
      Prelude.<*> (x Prelude..@? "from")

instance Prelude.Hashable PortRange

instance Prelude.NFData PortRange

instance Prelude.ToQuery PortRange where
  toQuery PortRange' {..} =
    Prelude.mconcat
      ["To" Prelude.=: to, "From" Prelude.=: from]
