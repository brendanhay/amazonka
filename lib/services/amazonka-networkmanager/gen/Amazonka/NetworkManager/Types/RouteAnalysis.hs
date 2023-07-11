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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysis where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.RouteAnalysisEndpointOptions
import Amazonka.NetworkManager.Types.RouteAnalysisPath
import Amazonka.NetworkManager.Types.RouteAnalysisStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a route analysis.
--
-- /See:/ 'newRouteAnalysis' smart constructor.
data RouteAnalysis = RouteAnalysis'
  { -- | The destination.
    destination :: Prelude.Maybe RouteAnalysisEndpointOptions,
    -- | The forward path.
    forwardPath :: Prelude.Maybe RouteAnalysisPath,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to analyze the return path. The return path is not
    -- analyzed if the forward path analysis does not succeed.
    includeReturnPath :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AWS account that created the route analysis.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The return path.
    returnPath :: Prelude.Maybe RouteAnalysisPath,
    -- | The ID of the route analysis.
    routeAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The source.
    source :: Prelude.Maybe RouteAnalysisEndpointOptions,
    -- | The time that the analysis started.
    startTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The status of the route analysis.
    status :: Prelude.Maybe RouteAnalysisStatus,
    -- | Indicates whether to include the location of middlebox appliances in the
    -- route analysis.
    useMiddleboxes :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'routeAnalysis_destination' - The destination.
--
-- 'forwardPath', 'routeAnalysis_forwardPath' - The forward path.
--
-- 'globalNetworkId', 'routeAnalysis_globalNetworkId' - The ID of the global network.
--
-- 'includeReturnPath', 'routeAnalysis_includeReturnPath' - Indicates whether to analyze the return path. The return path is not
-- analyzed if the forward path analysis does not succeed.
--
-- 'ownerAccountId', 'routeAnalysis_ownerAccountId' - The ID of the AWS account that created the route analysis.
--
-- 'returnPath', 'routeAnalysis_returnPath' - The return path.
--
-- 'routeAnalysisId', 'routeAnalysis_routeAnalysisId' - The ID of the route analysis.
--
-- 'source', 'routeAnalysis_source' - The source.
--
-- 'startTimestamp', 'routeAnalysis_startTimestamp' - The time that the analysis started.
--
-- 'status', 'routeAnalysis_status' - The status of the route analysis.
--
-- 'useMiddleboxes', 'routeAnalysis_useMiddleboxes' - Indicates whether to include the location of middlebox appliances in the
-- route analysis.
newRouteAnalysis ::
  RouteAnalysis
newRouteAnalysis =
  RouteAnalysis'
    { destination = Prelude.Nothing,
      forwardPath = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      includeReturnPath = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      returnPath = Prelude.Nothing,
      routeAnalysisId = Prelude.Nothing,
      source = Prelude.Nothing,
      startTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      useMiddleboxes = Prelude.Nothing
    }

-- | The destination.
routeAnalysis_destination :: Lens.Lens' RouteAnalysis (Prelude.Maybe RouteAnalysisEndpointOptions)
routeAnalysis_destination = Lens.lens (\RouteAnalysis' {destination} -> destination) (\s@RouteAnalysis' {} a -> s {destination = a} :: RouteAnalysis)

-- | The forward path.
routeAnalysis_forwardPath :: Lens.Lens' RouteAnalysis (Prelude.Maybe RouteAnalysisPath)
routeAnalysis_forwardPath = Lens.lens (\RouteAnalysis' {forwardPath} -> forwardPath) (\s@RouteAnalysis' {} a -> s {forwardPath = a} :: RouteAnalysis)

-- | The ID of the global network.
routeAnalysis_globalNetworkId :: Lens.Lens' RouteAnalysis (Prelude.Maybe Prelude.Text)
routeAnalysis_globalNetworkId = Lens.lens (\RouteAnalysis' {globalNetworkId} -> globalNetworkId) (\s@RouteAnalysis' {} a -> s {globalNetworkId = a} :: RouteAnalysis)

-- | Indicates whether to analyze the return path. The return path is not
-- analyzed if the forward path analysis does not succeed.
routeAnalysis_includeReturnPath :: Lens.Lens' RouteAnalysis (Prelude.Maybe Prelude.Bool)
routeAnalysis_includeReturnPath = Lens.lens (\RouteAnalysis' {includeReturnPath} -> includeReturnPath) (\s@RouteAnalysis' {} a -> s {includeReturnPath = a} :: RouteAnalysis)

-- | The ID of the AWS account that created the route analysis.
routeAnalysis_ownerAccountId :: Lens.Lens' RouteAnalysis (Prelude.Maybe Prelude.Text)
routeAnalysis_ownerAccountId = Lens.lens (\RouteAnalysis' {ownerAccountId} -> ownerAccountId) (\s@RouteAnalysis' {} a -> s {ownerAccountId = a} :: RouteAnalysis)

-- | The return path.
routeAnalysis_returnPath :: Lens.Lens' RouteAnalysis (Prelude.Maybe RouteAnalysisPath)
routeAnalysis_returnPath = Lens.lens (\RouteAnalysis' {returnPath} -> returnPath) (\s@RouteAnalysis' {} a -> s {returnPath = a} :: RouteAnalysis)

-- | The ID of the route analysis.
routeAnalysis_routeAnalysisId :: Lens.Lens' RouteAnalysis (Prelude.Maybe Prelude.Text)
routeAnalysis_routeAnalysisId = Lens.lens (\RouteAnalysis' {routeAnalysisId} -> routeAnalysisId) (\s@RouteAnalysis' {} a -> s {routeAnalysisId = a} :: RouteAnalysis)

-- | The source.
routeAnalysis_source :: Lens.Lens' RouteAnalysis (Prelude.Maybe RouteAnalysisEndpointOptions)
routeAnalysis_source = Lens.lens (\RouteAnalysis' {source} -> source) (\s@RouteAnalysis' {} a -> s {source = a} :: RouteAnalysis)

-- | The time that the analysis started.
routeAnalysis_startTimestamp :: Lens.Lens' RouteAnalysis (Prelude.Maybe Prelude.UTCTime)
routeAnalysis_startTimestamp = Lens.lens (\RouteAnalysis' {startTimestamp} -> startTimestamp) (\s@RouteAnalysis' {} a -> s {startTimestamp = a} :: RouteAnalysis) Prelude.. Lens.mapping Data._Time

-- | The status of the route analysis.
routeAnalysis_status :: Lens.Lens' RouteAnalysis (Prelude.Maybe RouteAnalysisStatus)
routeAnalysis_status = Lens.lens (\RouteAnalysis' {status} -> status) (\s@RouteAnalysis' {} a -> s {status = a} :: RouteAnalysis)

-- | Indicates whether to include the location of middlebox appliances in the
-- route analysis.
routeAnalysis_useMiddleboxes :: Lens.Lens' RouteAnalysis (Prelude.Maybe Prelude.Bool)
routeAnalysis_useMiddleboxes = Lens.lens (\RouteAnalysis' {useMiddleboxes} -> useMiddleboxes) (\s@RouteAnalysis' {} a -> s {useMiddleboxes = a} :: RouteAnalysis)

instance Data.FromJSON RouteAnalysis where
  parseJSON =
    Data.withObject
      "RouteAnalysis"
      ( \x ->
          RouteAnalysis'
            Prelude.<$> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "ForwardPath")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "IncludeReturnPath")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "ReturnPath")
            Prelude.<*> (x Data..:? "RouteAnalysisId")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "StartTimestamp")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UseMiddleboxes")
      )

instance Prelude.Hashable RouteAnalysis where
  hashWithSalt _salt RouteAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` forwardPath
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` includeReturnPath
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` returnPath
      `Prelude.hashWithSalt` routeAnalysisId
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` useMiddleboxes

instance Prelude.NFData RouteAnalysis where
  rnf RouteAnalysis' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf forwardPath
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf includeReturnPath
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf returnPath
      `Prelude.seq` Prelude.rnf routeAnalysisId
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf useMiddleboxes
