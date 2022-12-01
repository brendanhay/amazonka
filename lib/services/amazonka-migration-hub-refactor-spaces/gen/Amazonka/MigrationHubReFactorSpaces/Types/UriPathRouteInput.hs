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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.UriPathRouteInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.UriPathRouteInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubReFactorSpaces.Types.HttpMethod
import Amazonka.MigrationHubReFactorSpaces.Types.RouteActivationState
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the URI path route type.
--
-- /See:/ 'newUriPathRouteInput' smart constructor.
data UriPathRouteInput = UriPathRouteInput'
  { -- | A list of HTTP methods to match. An empty list matches all values. If a
    -- method is present, only HTTP requests using that method are forwarded to
    -- this route’s service.
    methods :: Prelude.Maybe [HttpMethod],
    -- | Indicates whether to match all subpaths of the given source path. If
    -- this value is @false@, requests must match the source path exactly
    -- before they are forwarded to this route\'s service.
    includeChildPaths :: Prelude.Maybe Prelude.Bool,
    -- | If set to @ACTIVE@, traffic is forwarded to this route’s service after
    -- the route is created.
    activationState :: RouteActivationState,
    -- | The path to use to match traffic. Paths must start with @\/@ and are
    -- relative to the base of the application.
    sourcePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UriPathRouteInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'methods', 'uriPathRouteInput_methods' - A list of HTTP methods to match. An empty list matches all values. If a
-- method is present, only HTTP requests using that method are forwarded to
-- this route’s service.
--
-- 'includeChildPaths', 'uriPathRouteInput_includeChildPaths' - Indicates whether to match all subpaths of the given source path. If
-- this value is @false@, requests must match the source path exactly
-- before they are forwarded to this route\'s service.
--
-- 'activationState', 'uriPathRouteInput_activationState' - If set to @ACTIVE@, traffic is forwarded to this route’s service after
-- the route is created.
--
-- 'sourcePath', 'uriPathRouteInput_sourcePath' - The path to use to match traffic. Paths must start with @\/@ and are
-- relative to the base of the application.
newUriPathRouteInput ::
  -- | 'activationState'
  RouteActivationState ->
  -- | 'sourcePath'
  Prelude.Text ->
  UriPathRouteInput
newUriPathRouteInput pActivationState_ pSourcePath_ =
  UriPathRouteInput'
    { methods = Prelude.Nothing,
      includeChildPaths = Prelude.Nothing,
      activationState = pActivationState_,
      sourcePath = pSourcePath_
    }

-- | A list of HTTP methods to match. An empty list matches all values. If a
-- method is present, only HTTP requests using that method are forwarded to
-- this route’s service.
uriPathRouteInput_methods :: Lens.Lens' UriPathRouteInput (Prelude.Maybe [HttpMethod])
uriPathRouteInput_methods = Lens.lens (\UriPathRouteInput' {methods} -> methods) (\s@UriPathRouteInput' {} a -> s {methods = a} :: UriPathRouteInput) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether to match all subpaths of the given source path. If
-- this value is @false@, requests must match the source path exactly
-- before they are forwarded to this route\'s service.
uriPathRouteInput_includeChildPaths :: Lens.Lens' UriPathRouteInput (Prelude.Maybe Prelude.Bool)
uriPathRouteInput_includeChildPaths = Lens.lens (\UriPathRouteInput' {includeChildPaths} -> includeChildPaths) (\s@UriPathRouteInput' {} a -> s {includeChildPaths = a} :: UriPathRouteInput)

-- | If set to @ACTIVE@, traffic is forwarded to this route’s service after
-- the route is created.
uriPathRouteInput_activationState :: Lens.Lens' UriPathRouteInput RouteActivationState
uriPathRouteInput_activationState = Lens.lens (\UriPathRouteInput' {activationState} -> activationState) (\s@UriPathRouteInput' {} a -> s {activationState = a} :: UriPathRouteInput)

-- | The path to use to match traffic. Paths must start with @\/@ and are
-- relative to the base of the application.
uriPathRouteInput_sourcePath :: Lens.Lens' UriPathRouteInput Prelude.Text
uriPathRouteInput_sourcePath = Lens.lens (\UriPathRouteInput' {sourcePath} -> sourcePath) (\s@UriPathRouteInput' {} a -> s {sourcePath = a} :: UriPathRouteInput)

instance Core.FromJSON UriPathRouteInput where
  parseJSON =
    Core.withObject
      "UriPathRouteInput"
      ( \x ->
          UriPathRouteInput'
            Prelude.<$> (x Core..:? "Methods" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IncludeChildPaths")
            Prelude.<*> (x Core..: "ActivationState")
            Prelude.<*> (x Core..: "SourcePath")
      )

instance Prelude.Hashable UriPathRouteInput where
  hashWithSalt _salt UriPathRouteInput' {..} =
    _salt `Prelude.hashWithSalt` methods
      `Prelude.hashWithSalt` includeChildPaths
      `Prelude.hashWithSalt` activationState
      `Prelude.hashWithSalt` sourcePath

instance Prelude.NFData UriPathRouteInput where
  rnf UriPathRouteInput' {..} =
    Prelude.rnf methods
      `Prelude.seq` Prelude.rnf includeChildPaths
      `Prelude.seq` Prelude.rnf activationState
      `Prelude.seq` Prelude.rnf sourcePath

instance Core.ToJSON UriPathRouteInput where
  toJSON UriPathRouteInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Methods" Core..=) Prelude.<$> methods,
            ("IncludeChildPaths" Core..=)
              Prelude.<$> includeChildPaths,
            Prelude.Just
              ("ActivationState" Core..= activationState),
            Prelude.Just ("SourcePath" Core..= sourcePath)
          ]
      )
