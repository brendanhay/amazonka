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
-- Module      : Amazonka.EC2.Types.NetworkCardInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkCardInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the network card support of the instance type.
--
-- /See:/ 'newNetworkCardInfo' smart constructor.
data NetworkCardInfo = NetworkCardInfo'
  { -- | The maximum number of network interfaces for the network card.
    maximumNetworkInterfaces :: Prelude.Maybe Prelude.Int,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The network performance of the network card.
    networkPerformance :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkCardInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumNetworkInterfaces', 'networkCardInfo_maximumNetworkInterfaces' - The maximum number of network interfaces for the network card.
--
-- 'networkCardIndex', 'networkCardInfo_networkCardIndex' - The index of the network card.
--
-- 'networkPerformance', 'networkCardInfo_networkPerformance' - The network performance of the network card.
newNetworkCardInfo ::
  NetworkCardInfo
newNetworkCardInfo =
  NetworkCardInfo'
    { maximumNetworkInterfaces =
        Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      networkPerformance = Prelude.Nothing
    }

-- | The maximum number of network interfaces for the network card.
networkCardInfo_maximumNetworkInterfaces :: Lens.Lens' NetworkCardInfo (Prelude.Maybe Prelude.Int)
networkCardInfo_maximumNetworkInterfaces = Lens.lens (\NetworkCardInfo' {maximumNetworkInterfaces} -> maximumNetworkInterfaces) (\s@NetworkCardInfo' {} a -> s {maximumNetworkInterfaces = a} :: NetworkCardInfo)

-- | The index of the network card.
networkCardInfo_networkCardIndex :: Lens.Lens' NetworkCardInfo (Prelude.Maybe Prelude.Int)
networkCardInfo_networkCardIndex = Lens.lens (\NetworkCardInfo' {networkCardIndex} -> networkCardIndex) (\s@NetworkCardInfo' {} a -> s {networkCardIndex = a} :: NetworkCardInfo)

-- | The network performance of the network card.
networkCardInfo_networkPerformance :: Lens.Lens' NetworkCardInfo (Prelude.Maybe Prelude.Text)
networkCardInfo_networkPerformance = Lens.lens (\NetworkCardInfo' {networkPerformance} -> networkPerformance) (\s@NetworkCardInfo' {} a -> s {networkPerformance = a} :: NetworkCardInfo)

instance Data.FromXML NetworkCardInfo where
  parseXML x =
    NetworkCardInfo'
      Prelude.<$> (x Data..@? "maximumNetworkInterfaces")
      Prelude.<*> (x Data..@? "networkCardIndex")
      Prelude.<*> (x Data..@? "networkPerformance")

instance Prelude.Hashable NetworkCardInfo where
  hashWithSalt _salt NetworkCardInfo' {..} =
    _salt
      `Prelude.hashWithSalt` maximumNetworkInterfaces
      `Prelude.hashWithSalt` networkCardIndex
      `Prelude.hashWithSalt` networkPerformance

instance Prelude.NFData NetworkCardInfo where
  rnf NetworkCardInfo' {..} =
    Prelude.rnf maximumNetworkInterfaces `Prelude.seq`
      Prelude.rnf networkCardIndex `Prelude.seq`
        Prelude.rnf networkPerformance
