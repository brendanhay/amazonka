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
-- Module      : Amazonka.Nimble.UpdateLaunchProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a launch profile.
module Amazonka.Nimble.UpdateLaunchProfile
  ( -- * Creating a Request
    UpdateLaunchProfile (..),
    newUpdateLaunchProfile,

    -- * Request Lenses
    updateLaunchProfile_studioComponentIds,
    updateLaunchProfile_name,
    updateLaunchProfile_clientToken,
    updateLaunchProfile_description,
    updateLaunchProfile_streamConfiguration,
    updateLaunchProfile_launchProfileProtocolVersions,
    updateLaunchProfile_studioId,
    updateLaunchProfile_launchProfileId,

    -- * Destructuring the Response
    UpdateLaunchProfileResponse (..),
    newUpdateLaunchProfileResponse,

    -- * Response Lenses
    updateLaunchProfileResponse_launchProfile,
    updateLaunchProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The launch profile ID.
--
-- /See:/ 'newUpdateLaunchProfile' smart constructor.
data UpdateLaunchProfile = UpdateLaunchProfile'
  { -- | Unique identifiers for a collection of studio components that can be
    -- used with this launch profile.
    studioComponentIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name for the launch profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A configuration for a streaming session.
    streamConfiguration :: Prelude.Maybe StreamConfigurationCreate,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersions :: Prelude.Maybe [Prelude.Text],
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponentIds', 'updateLaunchProfile_studioComponentIds' - Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
--
-- 'name', 'updateLaunchProfile_name' - The name for the launch profile.
--
-- 'clientToken', 'updateLaunchProfile_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'description', 'updateLaunchProfile_description' - The description.
--
-- 'streamConfiguration', 'updateLaunchProfile_streamConfiguration' - A configuration for a streaming session.
--
-- 'launchProfileProtocolVersions', 'updateLaunchProfile_launchProfileProtocolVersions' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'studioId', 'updateLaunchProfile_studioId' - The studio ID.
--
-- 'launchProfileId', 'updateLaunchProfile_launchProfileId' - The launch profile ID.
newUpdateLaunchProfile ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'launchProfileId'
  Prelude.Text ->
  UpdateLaunchProfile
newUpdateLaunchProfile pStudioId_ pLaunchProfileId_ =
  UpdateLaunchProfile'
    { studioComponentIds =
        Prelude.Nothing,
      name = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      streamConfiguration = Prelude.Nothing,
      launchProfileProtocolVersions = Prelude.Nothing,
      studioId = pStudioId_,
      launchProfileId = pLaunchProfileId_
    }

-- | Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
updateLaunchProfile_studioComponentIds :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLaunchProfile_studioComponentIds = Lens.lens (\UpdateLaunchProfile' {studioComponentIds} -> studioComponentIds) (\s@UpdateLaunchProfile' {} a -> s {studioComponentIds = a} :: UpdateLaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name for the launch profile.
updateLaunchProfile_name :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe Prelude.Text)
updateLaunchProfile_name = Lens.lens (\UpdateLaunchProfile' {name} -> name) (\s@UpdateLaunchProfile' {} a -> s {name = a} :: UpdateLaunchProfile)

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
updateLaunchProfile_clientToken :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe Prelude.Text)
updateLaunchProfile_clientToken = Lens.lens (\UpdateLaunchProfile' {clientToken} -> clientToken) (\s@UpdateLaunchProfile' {} a -> s {clientToken = a} :: UpdateLaunchProfile)

-- | The description.
updateLaunchProfile_description :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe Prelude.Text)
updateLaunchProfile_description = Lens.lens (\UpdateLaunchProfile' {description} -> description) (\s@UpdateLaunchProfile' {} a -> s {description = a} :: UpdateLaunchProfile)

-- | A configuration for a streaming session.
updateLaunchProfile_streamConfiguration :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe StreamConfigurationCreate)
updateLaunchProfile_streamConfiguration = Lens.lens (\UpdateLaunchProfile' {streamConfiguration} -> streamConfiguration) (\s@UpdateLaunchProfile' {} a -> s {streamConfiguration = a} :: UpdateLaunchProfile)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
updateLaunchProfile_launchProfileProtocolVersions :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe [Prelude.Text])
updateLaunchProfile_launchProfileProtocolVersions = Lens.lens (\UpdateLaunchProfile' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@UpdateLaunchProfile' {} a -> s {launchProfileProtocolVersions = a} :: UpdateLaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The studio ID.
updateLaunchProfile_studioId :: Lens.Lens' UpdateLaunchProfile Prelude.Text
updateLaunchProfile_studioId = Lens.lens (\UpdateLaunchProfile' {studioId} -> studioId) (\s@UpdateLaunchProfile' {} a -> s {studioId = a} :: UpdateLaunchProfile)

-- | The launch profile ID.
updateLaunchProfile_launchProfileId :: Lens.Lens' UpdateLaunchProfile Prelude.Text
updateLaunchProfile_launchProfileId = Lens.lens (\UpdateLaunchProfile' {launchProfileId} -> launchProfileId) (\s@UpdateLaunchProfile' {} a -> s {launchProfileId = a} :: UpdateLaunchProfile)

instance Core.AWSRequest UpdateLaunchProfile where
  type
    AWSResponse UpdateLaunchProfile =
      UpdateLaunchProfileResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLaunchProfileResponse'
            Prelude.<$> (x Core..?> "launchProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLaunchProfile where
  hashWithSalt _salt UpdateLaunchProfile' {..} =
    _salt `Prelude.hashWithSalt` studioComponentIds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` streamConfiguration
      `Prelude.hashWithSalt` launchProfileProtocolVersions
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` launchProfileId

instance Prelude.NFData UpdateLaunchProfile where
  rnf UpdateLaunchProfile' {..} =
    Prelude.rnf studioComponentIds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf streamConfiguration
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersions
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf launchProfileId

instance Core.ToHeaders UpdateLaunchProfile where
  toHeaders UpdateLaunchProfile' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON UpdateLaunchProfile where
  toJSON UpdateLaunchProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("studioComponentIds" Core..=)
              Prelude.<$> studioComponentIds,
            ("name" Core..=) Prelude.<$> name,
            ("description" Core..=) Prelude.<$> description,
            ("streamConfiguration" Core..=)
              Prelude.<$> streamConfiguration,
            ("launchProfileProtocolVersions" Core..=)
              Prelude.<$> launchProfileProtocolVersions
          ]
      )

instance Core.ToPath UpdateLaunchProfile where
  toPath UpdateLaunchProfile' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId
      ]

instance Core.ToQuery UpdateLaunchProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLaunchProfileResponse' smart constructor.
data UpdateLaunchProfileResponse = UpdateLaunchProfileResponse'
  { -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfile', 'updateLaunchProfileResponse_launchProfile' - The launch profile.
--
-- 'httpStatus', 'updateLaunchProfileResponse_httpStatus' - The response's http status code.
newUpdateLaunchProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLaunchProfileResponse
newUpdateLaunchProfileResponse pHttpStatus_ =
  UpdateLaunchProfileResponse'
    { launchProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The launch profile.
updateLaunchProfileResponse_launchProfile :: Lens.Lens' UpdateLaunchProfileResponse (Prelude.Maybe LaunchProfile)
updateLaunchProfileResponse_launchProfile = Lens.lens (\UpdateLaunchProfileResponse' {launchProfile} -> launchProfile) (\s@UpdateLaunchProfileResponse' {} a -> s {launchProfile = a} :: UpdateLaunchProfileResponse)

-- | The response's http status code.
updateLaunchProfileResponse_httpStatus :: Lens.Lens' UpdateLaunchProfileResponse Prelude.Int
updateLaunchProfileResponse_httpStatus = Lens.lens (\UpdateLaunchProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateLaunchProfileResponse' {} a -> s {httpStatus = a} :: UpdateLaunchProfileResponse)

instance Prelude.NFData UpdateLaunchProfileResponse where
  rnf UpdateLaunchProfileResponse' {..} =
    Prelude.rnf launchProfile
      `Prelude.seq` Prelude.rnf httpStatus
