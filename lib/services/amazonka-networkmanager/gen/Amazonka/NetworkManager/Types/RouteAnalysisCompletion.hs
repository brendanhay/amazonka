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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysisCompletion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysisCompletion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.RouteAnalysisCompletionReasonCode
import Amazonka.NetworkManager.Types.RouteAnalysisCompletionResultCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of an analysis at completion.
--
-- /See:/ 'newRouteAnalysisCompletion' smart constructor.
data RouteAnalysisCompletion = RouteAnalysisCompletion'
  { -- | The result of the analysis. If the status is @NOT_CONNECTED@, check the
    -- reason code.
    resultCode :: Prelude.Maybe RouteAnalysisCompletionResultCode,
    -- | The reason code. Available only if a connection is not found.
    --
    -- -   @BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND@ - Found a black hole route
    --     with the destination CIDR block.
    --
    -- -   @CYCLIC_PATH_DETECTED@ - Found the same resource multiple times
    --     while traversing the path.
    --
    -- -   @INACTIVE_ROUTE_FOR_DESTINATION_FOUND@ - Found an inactive route
    --     with the destination CIDR block.
    --
    -- -   @MAX_HOPS_EXCEEDED@ - Analysis exceeded 64 hops without finding the
    --     destination.
    --
    -- -   @ROUTE_NOT_FOUND@ - Cannot find a route table with the destination
    --     CIDR block.
    --
    -- -   @TGW_ATTACH_ARN_NO_MATCH@ - Found an attachment, but not with the
    --     correct destination ARN.
    --
    -- -   @TGW_ATTACH_NOT_FOUND@ - Cannot find an attachment.
    --
    -- -   @TGW_ATTACH_NOT_IN_TGW@ - Found an attachment, but not to the
    --     correct transit gateway.
    --
    -- -   @TGW_ATTACH_STABLE_ROUTE_TABLE_NOT_FOUND@ - The state of the route
    --     table association is not associated.
    reasonCode :: Prelude.Maybe RouteAnalysisCompletionReasonCode,
    -- | Additional information about the path. Available only if a connection is
    -- not found.
    reasonContext :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteAnalysisCompletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultCode', 'routeAnalysisCompletion_resultCode' - The result of the analysis. If the status is @NOT_CONNECTED@, check the
-- reason code.
--
-- 'reasonCode', 'routeAnalysisCompletion_reasonCode' - The reason code. Available only if a connection is not found.
--
-- -   @BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND@ - Found a black hole route
--     with the destination CIDR block.
--
-- -   @CYCLIC_PATH_DETECTED@ - Found the same resource multiple times
--     while traversing the path.
--
-- -   @INACTIVE_ROUTE_FOR_DESTINATION_FOUND@ - Found an inactive route
--     with the destination CIDR block.
--
-- -   @MAX_HOPS_EXCEEDED@ - Analysis exceeded 64 hops without finding the
--     destination.
--
-- -   @ROUTE_NOT_FOUND@ - Cannot find a route table with the destination
--     CIDR block.
--
-- -   @TGW_ATTACH_ARN_NO_MATCH@ - Found an attachment, but not with the
--     correct destination ARN.
--
-- -   @TGW_ATTACH_NOT_FOUND@ - Cannot find an attachment.
--
-- -   @TGW_ATTACH_NOT_IN_TGW@ - Found an attachment, but not to the
--     correct transit gateway.
--
-- -   @TGW_ATTACH_STABLE_ROUTE_TABLE_NOT_FOUND@ - The state of the route
--     table association is not associated.
--
-- 'reasonContext', 'routeAnalysisCompletion_reasonContext' - Additional information about the path. Available only if a connection is
-- not found.
newRouteAnalysisCompletion ::
  RouteAnalysisCompletion
newRouteAnalysisCompletion =
  RouteAnalysisCompletion'
    { resultCode =
        Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      reasonContext = Prelude.Nothing
    }

-- | The result of the analysis. If the status is @NOT_CONNECTED@, check the
-- reason code.
routeAnalysisCompletion_resultCode :: Lens.Lens' RouteAnalysisCompletion (Prelude.Maybe RouteAnalysisCompletionResultCode)
routeAnalysisCompletion_resultCode = Lens.lens (\RouteAnalysisCompletion' {resultCode} -> resultCode) (\s@RouteAnalysisCompletion' {} a -> s {resultCode = a} :: RouteAnalysisCompletion)

-- | The reason code. Available only if a connection is not found.
--
-- -   @BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND@ - Found a black hole route
--     with the destination CIDR block.
--
-- -   @CYCLIC_PATH_DETECTED@ - Found the same resource multiple times
--     while traversing the path.
--
-- -   @INACTIVE_ROUTE_FOR_DESTINATION_FOUND@ - Found an inactive route
--     with the destination CIDR block.
--
-- -   @MAX_HOPS_EXCEEDED@ - Analysis exceeded 64 hops without finding the
--     destination.
--
-- -   @ROUTE_NOT_FOUND@ - Cannot find a route table with the destination
--     CIDR block.
--
-- -   @TGW_ATTACH_ARN_NO_MATCH@ - Found an attachment, but not with the
--     correct destination ARN.
--
-- -   @TGW_ATTACH_NOT_FOUND@ - Cannot find an attachment.
--
-- -   @TGW_ATTACH_NOT_IN_TGW@ - Found an attachment, but not to the
--     correct transit gateway.
--
-- -   @TGW_ATTACH_STABLE_ROUTE_TABLE_NOT_FOUND@ - The state of the route
--     table association is not associated.
routeAnalysisCompletion_reasonCode :: Lens.Lens' RouteAnalysisCompletion (Prelude.Maybe RouteAnalysisCompletionReasonCode)
routeAnalysisCompletion_reasonCode = Lens.lens (\RouteAnalysisCompletion' {reasonCode} -> reasonCode) (\s@RouteAnalysisCompletion' {} a -> s {reasonCode = a} :: RouteAnalysisCompletion)

-- | Additional information about the path. Available only if a connection is
-- not found.
routeAnalysisCompletion_reasonContext :: Lens.Lens' RouteAnalysisCompletion (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routeAnalysisCompletion_reasonContext = Lens.lens (\RouteAnalysisCompletion' {reasonContext} -> reasonContext) (\s@RouteAnalysisCompletion' {} a -> s {reasonContext = a} :: RouteAnalysisCompletion) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON RouteAnalysisCompletion where
  parseJSON =
    Core.withObject
      "RouteAnalysisCompletion"
      ( \x ->
          RouteAnalysisCompletion'
            Prelude.<$> (x Core..:? "ResultCode")
            Prelude.<*> (x Core..:? "ReasonCode")
            Prelude.<*> (x Core..:? "ReasonContext" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable RouteAnalysisCompletion where
  hashWithSalt _salt RouteAnalysisCompletion' {..} =
    _salt `Prelude.hashWithSalt` resultCode
      `Prelude.hashWithSalt` reasonCode
      `Prelude.hashWithSalt` reasonContext

instance Prelude.NFData RouteAnalysisCompletion where
  rnf RouteAnalysisCompletion' {..} =
    Prelude.rnf resultCode
      `Prelude.seq` Prelude.rnf reasonCode
      `Prelude.seq` Prelude.rnf reasonContext
