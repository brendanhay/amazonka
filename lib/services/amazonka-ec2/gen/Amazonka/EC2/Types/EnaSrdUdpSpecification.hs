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
-- Module      : Amazonka.EC2.Types.EnaSrdUdpSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnaSrdUdpSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | ENA Express is compatible with both TCP and UDP transport protocols.
-- When it’s enabled, TCP traffic automatically uses it. However, some
-- UDP-based applications are designed to handle network packets that are
-- out of order, without a need for retransmission, such as live video
-- broadcasting or other near-real-time applications. For UDP traffic, you
-- can specify whether to use ENA Express, based on your application
-- environment needs.
--
-- /See:/ 'newEnaSrdUdpSpecification' smart constructor.
data EnaSrdUdpSpecification = EnaSrdUdpSpecification'
  { -- | Indicates whether UDP traffic uses ENA Express. To specify this setting,
    -- you must first enable ENA Express.
    enaSrdUdpEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnaSrdUdpSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enaSrdUdpEnabled', 'enaSrdUdpSpecification_enaSrdUdpEnabled' - Indicates whether UDP traffic uses ENA Express. To specify this setting,
-- you must first enable ENA Express.
newEnaSrdUdpSpecification ::
  EnaSrdUdpSpecification
newEnaSrdUdpSpecification =
  EnaSrdUdpSpecification'
    { enaSrdUdpEnabled =
        Prelude.Nothing
    }

-- | Indicates whether UDP traffic uses ENA Express. To specify this setting,
-- you must first enable ENA Express.
enaSrdUdpSpecification_enaSrdUdpEnabled :: Lens.Lens' EnaSrdUdpSpecification (Prelude.Maybe Prelude.Bool)
enaSrdUdpSpecification_enaSrdUdpEnabled = Lens.lens (\EnaSrdUdpSpecification' {enaSrdUdpEnabled} -> enaSrdUdpEnabled) (\s@EnaSrdUdpSpecification' {} a -> s {enaSrdUdpEnabled = a} :: EnaSrdUdpSpecification)

instance Prelude.Hashable EnaSrdUdpSpecification where
  hashWithSalt _salt EnaSrdUdpSpecification' {..} =
    _salt `Prelude.hashWithSalt` enaSrdUdpEnabled

instance Prelude.NFData EnaSrdUdpSpecification where
  rnf EnaSrdUdpSpecification' {..} =
    Prelude.rnf enaSrdUdpEnabled

instance Data.ToQuery EnaSrdUdpSpecification where
  toQuery EnaSrdUdpSpecification' {..} =
    Prelude.mconcat
      ["EnaSrdUdpEnabled" Data.=: enaSrdUdpEnabled]
