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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateLaunchProfile_launchProfileId,
    updateLaunchProfile_studioId,

    -- * Destructuring the Response
    UpdateLaunchProfileResponse (..),
    newUpdateLaunchProfileResponse,

    -- * Response Lenses
    updateLaunchProfileResponse_launchProfile,
    updateLaunchProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLaunchProfile' smart constructor.
data UpdateLaunchProfile = UpdateLaunchProfile'
  { -- | Unique identifiers for a collection of studio components that can be
    -- used with this launch profile.
    studioComponentIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name for the launch profile.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A configuration for a streaming session.
    streamConfiguration :: Prelude.Maybe StreamConfigurationCreate,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersions :: Prelude.Maybe [Prelude.Text],
    -- | The Launch Profile ID.
    launchProfileId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'clientToken', 'updateLaunchProfile_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'description', 'updateLaunchProfile_description' - The description.
--
-- 'streamConfiguration', 'updateLaunchProfile_streamConfiguration' - A configuration for a streaming session.
--
-- 'launchProfileProtocolVersions', 'updateLaunchProfile_launchProfileProtocolVersions' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'launchProfileId', 'updateLaunchProfile_launchProfileId' - The Launch Profile ID.
--
-- 'studioId', 'updateLaunchProfile_studioId' - The studio ID.
newUpdateLaunchProfile ::
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  UpdateLaunchProfile
newUpdateLaunchProfile pLaunchProfileId_ pStudioId_ =
  UpdateLaunchProfile'
    { studioComponentIds =
        Prelude.Nothing,
      name = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      streamConfiguration = Prelude.Nothing,
      launchProfileProtocolVersions = Prelude.Nothing,
      launchProfileId = pLaunchProfileId_,
      studioId = pStudioId_
    }

-- | Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
updateLaunchProfile_studioComponentIds :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLaunchProfile_studioComponentIds = Lens.lens (\UpdateLaunchProfile' {studioComponentIds} -> studioComponentIds) (\s@UpdateLaunchProfile' {} a -> s {studioComponentIds = a} :: UpdateLaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name for the launch profile.
updateLaunchProfile_name :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe Prelude.Text)
updateLaunchProfile_name = Lens.lens (\UpdateLaunchProfile' {name} -> name) (\s@UpdateLaunchProfile' {} a -> s {name = a} :: UpdateLaunchProfile) Prelude.. Lens.mapping Core._Sensitive

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
updateLaunchProfile_clientToken :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe Prelude.Text)
updateLaunchProfile_clientToken = Lens.lens (\UpdateLaunchProfile' {clientToken} -> clientToken) (\s@UpdateLaunchProfile' {} a -> s {clientToken = a} :: UpdateLaunchProfile)

-- | The description.
updateLaunchProfile_description :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe Prelude.Text)
updateLaunchProfile_description = Lens.lens (\UpdateLaunchProfile' {description} -> description) (\s@UpdateLaunchProfile' {} a -> s {description = a} :: UpdateLaunchProfile) Prelude.. Lens.mapping Core._Sensitive

-- | A configuration for a streaming session.
updateLaunchProfile_streamConfiguration :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe StreamConfigurationCreate)
updateLaunchProfile_streamConfiguration = Lens.lens (\UpdateLaunchProfile' {streamConfiguration} -> streamConfiguration) (\s@UpdateLaunchProfile' {} a -> s {streamConfiguration = a} :: UpdateLaunchProfile)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
updateLaunchProfile_launchProfileProtocolVersions :: Lens.Lens' UpdateLaunchProfile (Prelude.Maybe [Prelude.Text])
updateLaunchProfile_launchProfileProtocolVersions = Lens.lens (\UpdateLaunchProfile' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@UpdateLaunchProfile' {} a -> s {launchProfileProtocolVersions = a} :: UpdateLaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The Launch Profile ID.
updateLaunchProfile_launchProfileId :: Lens.Lens' UpdateLaunchProfile Prelude.Text
updateLaunchProfile_launchProfileId = Lens.lens (\UpdateLaunchProfile' {launchProfileId} -> launchProfileId) (\s@UpdateLaunchProfile' {} a -> s {launchProfileId = a} :: UpdateLaunchProfile)

-- | The studio ID.
updateLaunchProfile_studioId :: Lens.Lens' UpdateLaunchProfile Prelude.Text
updateLaunchProfile_studioId = Lens.lens (\UpdateLaunchProfile' {studioId} -> studioId) (\s@UpdateLaunchProfile' {} a -> s {studioId = a} :: UpdateLaunchProfile)

instance Core.AWSRequest UpdateLaunchProfile where
  type
    AWSResponse UpdateLaunchProfile =
      UpdateLaunchProfileResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
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
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData UpdateLaunchProfile where
  rnf UpdateLaunchProfile' {..} =
    Prelude.rnf studioComponentIds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf streamConfiguration
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersions
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf studioId

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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
