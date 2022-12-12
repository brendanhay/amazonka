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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.RouteSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.RouteSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
import Amazonka.MigrationHubReFactorSpaces.Types.HttpMethod
import Amazonka.MigrationHubReFactorSpaces.Types.RouteState
import Amazonka.MigrationHubReFactorSpaces.Types.RouteType
import qualified Amazonka.Prelude as Prelude

-- | The summary information for the routes as a response to @ListRoutes@.
--
-- /See:/ 'newRouteSummary' smart constructor.
data RouteSummary = RouteSummary'
  { -- | The unique identifier of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the route.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the route creator.
    createdByAccountId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the route is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the route resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | Indicates whether to match all subpaths of the given source path. If
    -- this value is @false@, requests must match the source path exactly
    -- before they are forwarded to this route\'s service.
    includeChildPaths :: Prelude.Maybe Prelude.Bool,
    -- | A timestamp that indicates when the route was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | A list of HTTP methods to match. An empty list matches all values. If a
    -- method is present, only HTTP requests using that method are forwarded to
    -- this route’s service.
    methods :: Prelude.Maybe [HttpMethod],
    -- | The Amazon Web Services account ID of the route owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | A mapping of Amazon API Gateway path resources to resource IDs.
    pathResourceToId :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier of the route.
    routeId :: Prelude.Maybe Prelude.Text,
    -- | The route type of the route.
    routeType :: Prelude.Maybe RouteType,
    -- | The unique identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The path to use to match traffic. Paths must start with @\/@ and are
    -- relative to the base of the application.
    sourcePath :: Prelude.Maybe Prelude.Text,
    -- | The current state of the route.
    state :: Prelude.Maybe RouteState,
    -- | The tags assigned to the route.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'routeSummary_applicationId' - The unique identifier of the application.
--
-- 'arn', 'routeSummary_arn' - The Amazon Resource Name (ARN) of the route.
--
-- 'createdByAccountId', 'routeSummary_createdByAccountId' - The Amazon Web Services account ID of the route creator.
--
-- 'createdTime', 'routeSummary_createdTime' - A timestamp that indicates when the route is created.
--
-- 'environmentId', 'routeSummary_environmentId' - The unique identifier of the environment.
--
-- 'error', 'routeSummary_error' - Any error associated with the route resource.
--
-- 'includeChildPaths', 'routeSummary_includeChildPaths' - Indicates whether to match all subpaths of the given source path. If
-- this value is @false@, requests must match the source path exactly
-- before they are forwarded to this route\'s service.
--
-- 'lastUpdatedTime', 'routeSummary_lastUpdatedTime' - A timestamp that indicates when the route was last updated.
--
-- 'methods', 'routeSummary_methods' - A list of HTTP methods to match. An empty list matches all values. If a
-- method is present, only HTTP requests using that method are forwarded to
-- this route’s service.
--
-- 'ownerAccountId', 'routeSummary_ownerAccountId' - The Amazon Web Services account ID of the route owner.
--
-- 'pathResourceToId', 'routeSummary_pathResourceToId' - A mapping of Amazon API Gateway path resources to resource IDs.
--
-- 'routeId', 'routeSummary_routeId' - The unique identifier of the route.
--
-- 'routeType', 'routeSummary_routeType' - The route type of the route.
--
-- 'serviceId', 'routeSummary_serviceId' - The unique identifier of the service.
--
-- 'sourcePath', 'routeSummary_sourcePath' - The path to use to match traffic. Paths must start with @\/@ and are
-- relative to the base of the application.
--
-- 'state', 'routeSummary_state' - The current state of the route.
--
-- 'tags', 'routeSummary_tags' - The tags assigned to the route.
newRouteSummary ::
  RouteSummary
newRouteSummary =
  RouteSummary'
    { applicationId = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdByAccountId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
      includeChildPaths = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      methods = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      pathResourceToId = Prelude.Nothing,
      routeId = Prelude.Nothing,
      routeType = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      sourcePath = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The unique identifier of the application.
routeSummary_applicationId :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_applicationId = Lens.lens (\RouteSummary' {applicationId} -> applicationId) (\s@RouteSummary' {} a -> s {applicationId = a} :: RouteSummary)

-- | The Amazon Resource Name (ARN) of the route.
routeSummary_arn :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_arn = Lens.lens (\RouteSummary' {arn} -> arn) (\s@RouteSummary' {} a -> s {arn = a} :: RouteSummary)

-- | The Amazon Web Services account ID of the route creator.
routeSummary_createdByAccountId :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_createdByAccountId = Lens.lens (\RouteSummary' {createdByAccountId} -> createdByAccountId) (\s@RouteSummary' {} a -> s {createdByAccountId = a} :: RouteSummary)

-- | A timestamp that indicates when the route is created.
routeSummary_createdTime :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.UTCTime)
routeSummary_createdTime = Lens.lens (\RouteSummary' {createdTime} -> createdTime) (\s@RouteSummary' {} a -> s {createdTime = a} :: RouteSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the environment.
routeSummary_environmentId :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_environmentId = Lens.lens (\RouteSummary' {environmentId} -> environmentId) (\s@RouteSummary' {} a -> s {environmentId = a} :: RouteSummary)

-- | Any error associated with the route resource.
routeSummary_error :: Lens.Lens' RouteSummary (Prelude.Maybe ErrorResponse)
routeSummary_error = Lens.lens (\RouteSummary' {error} -> error) (\s@RouteSummary' {} a -> s {error = a} :: RouteSummary)

-- | Indicates whether to match all subpaths of the given source path. If
-- this value is @false@, requests must match the source path exactly
-- before they are forwarded to this route\'s service.
routeSummary_includeChildPaths :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Bool)
routeSummary_includeChildPaths = Lens.lens (\RouteSummary' {includeChildPaths} -> includeChildPaths) (\s@RouteSummary' {} a -> s {includeChildPaths = a} :: RouteSummary)

-- | A timestamp that indicates when the route was last updated.
routeSummary_lastUpdatedTime :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.UTCTime)
routeSummary_lastUpdatedTime = Lens.lens (\RouteSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@RouteSummary' {} a -> s {lastUpdatedTime = a} :: RouteSummary) Prelude.. Lens.mapping Data._Time

-- | A list of HTTP methods to match. An empty list matches all values. If a
-- method is present, only HTTP requests using that method are forwarded to
-- this route’s service.
routeSummary_methods :: Lens.Lens' RouteSummary (Prelude.Maybe [HttpMethod])
routeSummary_methods = Lens.lens (\RouteSummary' {methods} -> methods) (\s@RouteSummary' {} a -> s {methods = a} :: RouteSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID of the route owner.
routeSummary_ownerAccountId :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_ownerAccountId = Lens.lens (\RouteSummary' {ownerAccountId} -> ownerAccountId) (\s@RouteSummary' {} a -> s {ownerAccountId = a} :: RouteSummary)

-- | A mapping of Amazon API Gateway path resources to resource IDs.
routeSummary_pathResourceToId :: Lens.Lens' RouteSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routeSummary_pathResourceToId = Lens.lens (\RouteSummary' {pathResourceToId} -> pathResourceToId) (\s@RouteSummary' {} a -> s {pathResourceToId = a} :: RouteSummary) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the route.
routeSummary_routeId :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_routeId = Lens.lens (\RouteSummary' {routeId} -> routeId) (\s@RouteSummary' {} a -> s {routeId = a} :: RouteSummary)

-- | The route type of the route.
routeSummary_routeType :: Lens.Lens' RouteSummary (Prelude.Maybe RouteType)
routeSummary_routeType = Lens.lens (\RouteSummary' {routeType} -> routeType) (\s@RouteSummary' {} a -> s {routeType = a} :: RouteSummary)

-- | The unique identifier of the service.
routeSummary_serviceId :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_serviceId = Lens.lens (\RouteSummary' {serviceId} -> serviceId) (\s@RouteSummary' {} a -> s {serviceId = a} :: RouteSummary)

-- | The path to use to match traffic. Paths must start with @\/@ and are
-- relative to the base of the application.
routeSummary_sourcePath :: Lens.Lens' RouteSummary (Prelude.Maybe Prelude.Text)
routeSummary_sourcePath = Lens.lens (\RouteSummary' {sourcePath} -> sourcePath) (\s@RouteSummary' {} a -> s {sourcePath = a} :: RouteSummary)

-- | The current state of the route.
routeSummary_state :: Lens.Lens' RouteSummary (Prelude.Maybe RouteState)
routeSummary_state = Lens.lens (\RouteSummary' {state} -> state) (\s@RouteSummary' {} a -> s {state = a} :: RouteSummary)

-- | The tags assigned to the route.
routeSummary_tags :: Lens.Lens' RouteSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routeSummary_tags = Lens.lens (\RouteSummary' {tags} -> tags) (\s@RouteSummary' {} a -> s {tags = a} :: RouteSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON RouteSummary where
  parseJSON =
    Data.withObject
      "RouteSummary"
      ( \x ->
          RouteSummary'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedByAccountId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "IncludeChildPaths")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Methods" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> ( x Data..:? "PathResourceToId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RouteId")
            Prelude.<*> (x Data..:? "RouteType")
            Prelude.<*> (x Data..:? "ServiceId")
            Prelude.<*> (x Data..:? "SourcePath")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RouteSummary where
  hashWithSalt _salt RouteSummary' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdByAccountId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` includeChildPaths
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` methods
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` pathResourceToId
      `Prelude.hashWithSalt` routeId
      `Prelude.hashWithSalt` routeType
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` sourcePath
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData RouteSummary where
  rnf RouteSummary' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdByAccountId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf includeChildPaths
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf methods
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf pathResourceToId
      `Prelude.seq` Prelude.rnf routeId
      `Prelude.seq` Prelude.rnf routeType
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf sourcePath
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
