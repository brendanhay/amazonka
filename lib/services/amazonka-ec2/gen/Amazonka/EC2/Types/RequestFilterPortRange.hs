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
-- Module      : Amazonka.EC2.Types.RequestFilterPortRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RequestFilterPortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a port range.
--
-- /See:/ 'newRequestFilterPortRange' smart constructor.
data RequestFilterPortRange = RequestFilterPortRange'
  { -- | The first port in the range.
    fromPort :: Prelude.Maybe Prelude.Natural,
    -- | The last port in the range.
    toPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestFilterPortRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'requestFilterPortRange_fromPort' - The first port in the range.
--
-- 'toPort', 'requestFilterPortRange_toPort' - The last port in the range.
newRequestFilterPortRange ::
  RequestFilterPortRange
newRequestFilterPortRange =
  RequestFilterPortRange'
    { fromPort = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The first port in the range.
requestFilterPortRange_fromPort :: Lens.Lens' RequestFilterPortRange (Prelude.Maybe Prelude.Natural)
requestFilterPortRange_fromPort = Lens.lens (\RequestFilterPortRange' {fromPort} -> fromPort) (\s@RequestFilterPortRange' {} a -> s {fromPort = a} :: RequestFilterPortRange)

-- | The last port in the range.
requestFilterPortRange_toPort :: Lens.Lens' RequestFilterPortRange (Prelude.Maybe Prelude.Natural)
requestFilterPortRange_toPort = Lens.lens (\RequestFilterPortRange' {toPort} -> toPort) (\s@RequestFilterPortRange' {} a -> s {toPort = a} :: RequestFilterPortRange)

instance Prelude.Hashable RequestFilterPortRange where
  hashWithSalt _salt RequestFilterPortRange' {..} =
    _salt
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` toPort

instance Prelude.NFData RequestFilterPortRange where
  rnf RequestFilterPortRange' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf toPort

instance Data.ToQuery RequestFilterPortRange where
  toQuery RequestFilterPortRange' {..} =
    Prelude.mconcat
      [ "FromPort" Data.=: fromPort,
        "ToPort" Data.=: toPort
      ]
