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
-- Module      : Amazonka.EC2.Types.EnaSrdSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnaSrdSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EnaSrdUdpSpecification
import qualified Amazonka.Prelude as Prelude

-- | ENA Express uses Amazon Web Services Scalable Reliable Datagram (SRD)
-- technology to increase the maximum bandwidth used per stream and
-- minimize tail latency of network traffic between EC2 instances. With ENA
-- Express, you can communicate between two EC2 instances in the same
-- subnet within the same account, or in different accounts. Both sending
-- and receiving instances must have ENA Express enabled.
--
-- To improve the reliability of network packet delivery, ENA Express
-- reorders network packets on the receiving end by default. However, some
-- UDP-based applications are designed to handle network packets that are
-- out of order to reduce the overhead for packet delivery at the network
-- layer. When ENA Express is enabled, you can specify whether UDP network
-- traffic uses it.
--
-- /See:/ 'newEnaSrdSpecification' smart constructor.
data EnaSrdSpecification = EnaSrdSpecification'
  { -- | Indicates whether ENA Express is enabled for the network interface.
    enaSrdEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Configures ENA Express for UDP network traffic.
    enaSrdUdpSpecification :: Prelude.Maybe EnaSrdUdpSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnaSrdSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enaSrdEnabled', 'enaSrdSpecification_enaSrdEnabled' - Indicates whether ENA Express is enabled for the network interface.
--
-- 'enaSrdUdpSpecification', 'enaSrdSpecification_enaSrdUdpSpecification' - Configures ENA Express for UDP network traffic.
newEnaSrdSpecification ::
  EnaSrdSpecification
newEnaSrdSpecification =
  EnaSrdSpecification'
    { enaSrdEnabled =
        Prelude.Nothing,
      enaSrdUdpSpecification = Prelude.Nothing
    }

-- | Indicates whether ENA Express is enabled for the network interface.
enaSrdSpecification_enaSrdEnabled :: Lens.Lens' EnaSrdSpecification (Prelude.Maybe Prelude.Bool)
enaSrdSpecification_enaSrdEnabled = Lens.lens (\EnaSrdSpecification' {enaSrdEnabled} -> enaSrdEnabled) (\s@EnaSrdSpecification' {} a -> s {enaSrdEnabled = a} :: EnaSrdSpecification)

-- | Configures ENA Express for UDP network traffic.
enaSrdSpecification_enaSrdUdpSpecification :: Lens.Lens' EnaSrdSpecification (Prelude.Maybe EnaSrdUdpSpecification)
enaSrdSpecification_enaSrdUdpSpecification = Lens.lens (\EnaSrdSpecification' {enaSrdUdpSpecification} -> enaSrdUdpSpecification) (\s@EnaSrdSpecification' {} a -> s {enaSrdUdpSpecification = a} :: EnaSrdSpecification)

instance Prelude.Hashable EnaSrdSpecification where
  hashWithSalt _salt EnaSrdSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` enaSrdEnabled
      `Prelude.hashWithSalt` enaSrdUdpSpecification

instance Prelude.NFData EnaSrdSpecification where
  rnf EnaSrdSpecification' {..} =
    Prelude.rnf enaSrdEnabled
      `Prelude.seq` Prelude.rnf enaSrdUdpSpecification

instance Data.ToQuery EnaSrdSpecification where
  toQuery EnaSrdSpecification' {..} =
    Prelude.mconcat
      [ "EnaSrdEnabled" Data.=: enaSrdEnabled,
        "EnaSrdUdpSpecification"
          Data.=: enaSrdUdpSpecification
      ]
