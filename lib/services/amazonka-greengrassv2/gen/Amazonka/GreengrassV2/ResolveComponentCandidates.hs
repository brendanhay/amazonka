{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GreengrassV2.ResolveComponentCandidates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of components that meet the component, version, and
-- platform requirements of a deployment. Greengrass core devices call this
-- operation when they receive a deployment to identify the components to
-- install.
--
-- This operation identifies components that meet all dependency
-- requirements for a deployment. If the requirements conflict, then this
-- operation returns an error and the deployment fails. For example, this
-- occurs if component @A@ requires version @>2.0.0@ and component @B@
-- requires version @\<2.0.0@ of a component dependency.
--
-- When you specify the component candidates to resolve, IoT Greengrass
-- compares each component\'s digest from the core device with the
-- component\'s digest in the Amazon Web Services Cloud. If the digests
-- don\'t match, then IoT Greengrass specifies to use the version from the
-- Amazon Web Services Cloud.
--
-- To use this operation, you must use the data plane API endpoint and
-- authenticate with an IoT device certificate. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/greengrass.html IoT Greengrass endpoints and quotas>.
module Amazonka.GreengrassV2.ResolveComponentCandidates
  ( -- * Creating a Request
    ResolveComponentCandidates (..),
    newResolveComponentCandidates,

    -- * Request Lenses
    resolveComponentCandidates_platform,
    resolveComponentCandidates_componentCandidates,

    -- * Destructuring the Response
    ResolveComponentCandidatesResponse (..),
    newResolveComponentCandidatesResponse,

    -- * Response Lenses
    resolveComponentCandidatesResponse_resolvedComponentVersions,
    resolveComponentCandidatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResolveComponentCandidates' smart constructor.
data ResolveComponentCandidates = ResolveComponentCandidates'
  { -- | The platform to use to resolve compatible components.
    platform :: Prelude.Maybe ComponentPlatform,
    -- | The list of components to resolve.
    componentCandidates :: Prelude.Maybe [ComponentCandidate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveComponentCandidates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'resolveComponentCandidates_platform' - The platform to use to resolve compatible components.
--
-- 'componentCandidates', 'resolveComponentCandidates_componentCandidates' - The list of components to resolve.
newResolveComponentCandidates ::
  ResolveComponentCandidates
newResolveComponentCandidates =
  ResolveComponentCandidates'
    { platform =
        Prelude.Nothing,
      componentCandidates = Prelude.Nothing
    }

-- | The platform to use to resolve compatible components.
resolveComponentCandidates_platform :: Lens.Lens' ResolveComponentCandidates (Prelude.Maybe ComponentPlatform)
resolveComponentCandidates_platform = Lens.lens (\ResolveComponentCandidates' {platform} -> platform) (\s@ResolveComponentCandidates' {} a -> s {platform = a} :: ResolveComponentCandidates)

-- | The list of components to resolve.
resolveComponentCandidates_componentCandidates :: Lens.Lens' ResolveComponentCandidates (Prelude.Maybe [ComponentCandidate])
resolveComponentCandidates_componentCandidates = Lens.lens (\ResolveComponentCandidates' {componentCandidates} -> componentCandidates) (\s@ResolveComponentCandidates' {} a -> s {componentCandidates = a} :: ResolveComponentCandidates) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ResolveComponentCandidates where
  type
    AWSResponse ResolveComponentCandidates =
      ResolveComponentCandidatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveComponentCandidatesResponse'
            Prelude.<$> ( x Core..?> "resolvedComponentVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveComponentCandidates where
  hashWithSalt _salt ResolveComponentCandidates' {..} =
    _salt `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` componentCandidates

instance Prelude.NFData ResolveComponentCandidates where
  rnf ResolveComponentCandidates' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf componentCandidates

instance Core.ToHeaders ResolveComponentCandidates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ResolveComponentCandidates where
  toJSON ResolveComponentCandidates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("platform" Core..=) Prelude.<$> platform,
            ("componentCandidates" Core..=)
              Prelude.<$> componentCandidates
          ]
      )

instance Core.ToPath ResolveComponentCandidates where
  toPath =
    Prelude.const
      "/greengrass/v2/resolveComponentCandidates"

instance Core.ToQuery ResolveComponentCandidates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResolveComponentCandidatesResponse' smart constructor.
data ResolveComponentCandidatesResponse = ResolveComponentCandidatesResponse'
  { -- | A list of components that meet the requirements that you specify in the
    -- request. This list includes each component\'s recipe that you can use to
    -- install the component.
    resolvedComponentVersions :: Prelude.Maybe [ResolvedComponentVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveComponentCandidatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolvedComponentVersions', 'resolveComponentCandidatesResponse_resolvedComponentVersions' - A list of components that meet the requirements that you specify in the
-- request. This list includes each component\'s recipe that you can use to
-- install the component.
--
-- 'httpStatus', 'resolveComponentCandidatesResponse_httpStatus' - The response's http status code.
newResolveComponentCandidatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResolveComponentCandidatesResponse
newResolveComponentCandidatesResponse pHttpStatus_ =
  ResolveComponentCandidatesResponse'
    { resolvedComponentVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of components that meet the requirements that you specify in the
-- request. This list includes each component\'s recipe that you can use to
-- install the component.
resolveComponentCandidatesResponse_resolvedComponentVersions :: Lens.Lens' ResolveComponentCandidatesResponse (Prelude.Maybe [ResolvedComponentVersion])
resolveComponentCandidatesResponse_resolvedComponentVersions = Lens.lens (\ResolveComponentCandidatesResponse' {resolvedComponentVersions} -> resolvedComponentVersions) (\s@ResolveComponentCandidatesResponse' {} a -> s {resolvedComponentVersions = a} :: ResolveComponentCandidatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
resolveComponentCandidatesResponse_httpStatus :: Lens.Lens' ResolveComponentCandidatesResponse Prelude.Int
resolveComponentCandidatesResponse_httpStatus = Lens.lens (\ResolveComponentCandidatesResponse' {httpStatus} -> httpStatus) (\s@ResolveComponentCandidatesResponse' {} a -> s {httpStatus = a} :: ResolveComponentCandidatesResponse)

instance
  Prelude.NFData
    ResolveComponentCandidatesResponse
  where
  rnf ResolveComponentCandidatesResponse' {..} =
    Prelude.rnf resolvedComponentVersions
      `Prelude.seq` Prelude.rnf httpStatus
