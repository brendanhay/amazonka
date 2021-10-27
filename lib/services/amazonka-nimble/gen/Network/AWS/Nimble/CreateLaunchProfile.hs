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
-- Module      : Network.AWS.Nimble.CreateLaunchProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a launch profile.
module Network.AWS.Nimble.CreateLaunchProfile
  ( -- * Creating a Request
    CreateLaunchProfile (..),
    newCreateLaunchProfile,

    -- * Request Lenses
    createLaunchProfile_clientToken,
    createLaunchProfile_description,
    createLaunchProfile_tags,
    createLaunchProfile_ec2SubnetIds,
    createLaunchProfile_studioComponentIds,
    createLaunchProfile_studioId,
    createLaunchProfile_launchProfileProtocolVersions,
    createLaunchProfile_name,
    createLaunchProfile_streamConfiguration,

    -- * Destructuring the Response
    CreateLaunchProfileResponse (..),
    newCreateLaunchProfileResponse,

    -- * Response Lenses
    createLaunchProfileResponse_launchProfile,
    createLaunchProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A collection of launch profiles.
--
-- /See:/ 'newCreateLaunchProfile' smart constructor.
data CreateLaunchProfile = CreateLaunchProfile'
  { -- | To make an idempotent API request using one of these actions, specify a
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
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    ec2SubnetIds :: [Prelude.Text],
    -- | Unique identifiers for a collection of studio components that can be
    -- used with this launch profile.
    studioComponentIds :: Prelude.NonEmpty Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersions :: [Prelude.Text],
    -- | The name for the launch profile.
    name :: Prelude.Text,
    -- | A configuration for a streaming session.
    streamConfiguration :: StreamConfigurationCreate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createLaunchProfile_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'description', 'createLaunchProfile_description' - The description.
--
-- 'tags', 'createLaunchProfile_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'ec2SubnetIds', 'createLaunchProfile_ec2SubnetIds' -
--
-- 'studioComponentIds', 'createLaunchProfile_studioComponentIds' - Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
--
-- 'studioId', 'createLaunchProfile_studioId' - The studio ID.
--
-- 'launchProfileProtocolVersions', 'createLaunchProfile_launchProfileProtocolVersions' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'name', 'createLaunchProfile_name' - The name for the launch profile.
--
-- 'streamConfiguration', 'createLaunchProfile_streamConfiguration' - A configuration for a streaming session.
newCreateLaunchProfile ::
  -- | 'studioComponentIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'streamConfiguration'
  StreamConfigurationCreate ->
  CreateLaunchProfile
newCreateLaunchProfile
  pStudioComponentIds_
  pStudioId_
  pName_
  pStreamConfiguration_ =
    CreateLaunchProfile'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        ec2SubnetIds = Prelude.mempty,
        studioComponentIds =
          Lens.coerced Lens.# pStudioComponentIds_,
        studioId = pStudioId_,
        launchProfileProtocolVersions = Prelude.mempty,
        name = pName_,
        streamConfiguration = pStreamConfiguration_
      }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
createLaunchProfile_clientToken :: Lens.Lens' CreateLaunchProfile (Prelude.Maybe Prelude.Text)
createLaunchProfile_clientToken = Lens.lens (\CreateLaunchProfile' {clientToken} -> clientToken) (\s@CreateLaunchProfile' {} a -> s {clientToken = a} :: CreateLaunchProfile)

-- | The description.
createLaunchProfile_description :: Lens.Lens' CreateLaunchProfile (Prelude.Maybe Prelude.Text)
createLaunchProfile_description = Lens.lens (\CreateLaunchProfile' {description} -> description) (\s@CreateLaunchProfile' {} a -> s {description = a} :: CreateLaunchProfile)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createLaunchProfile_tags :: Lens.Lens' CreateLaunchProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLaunchProfile_tags = Lens.lens (\CreateLaunchProfile' {tags} -> tags) (\s@CreateLaunchProfile' {} a -> s {tags = a} :: CreateLaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- |
createLaunchProfile_ec2SubnetIds :: Lens.Lens' CreateLaunchProfile [Prelude.Text]
createLaunchProfile_ec2SubnetIds = Lens.lens (\CreateLaunchProfile' {ec2SubnetIds} -> ec2SubnetIds) (\s@CreateLaunchProfile' {} a -> s {ec2SubnetIds = a} :: CreateLaunchProfile) Prelude.. Lens.coerced

-- | Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
createLaunchProfile_studioComponentIds :: Lens.Lens' CreateLaunchProfile (Prelude.NonEmpty Prelude.Text)
createLaunchProfile_studioComponentIds = Lens.lens (\CreateLaunchProfile' {studioComponentIds} -> studioComponentIds) (\s@CreateLaunchProfile' {} a -> s {studioComponentIds = a} :: CreateLaunchProfile) Prelude.. Lens.coerced

-- | The studio ID.
createLaunchProfile_studioId :: Lens.Lens' CreateLaunchProfile Prelude.Text
createLaunchProfile_studioId = Lens.lens (\CreateLaunchProfile' {studioId} -> studioId) (\s@CreateLaunchProfile' {} a -> s {studioId = a} :: CreateLaunchProfile)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
createLaunchProfile_launchProfileProtocolVersions :: Lens.Lens' CreateLaunchProfile [Prelude.Text]
createLaunchProfile_launchProfileProtocolVersions = Lens.lens (\CreateLaunchProfile' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@CreateLaunchProfile' {} a -> s {launchProfileProtocolVersions = a} :: CreateLaunchProfile) Prelude.. Lens.coerced

-- | The name for the launch profile.
createLaunchProfile_name :: Lens.Lens' CreateLaunchProfile Prelude.Text
createLaunchProfile_name = Lens.lens (\CreateLaunchProfile' {name} -> name) (\s@CreateLaunchProfile' {} a -> s {name = a} :: CreateLaunchProfile)

-- | A configuration for a streaming session.
createLaunchProfile_streamConfiguration :: Lens.Lens' CreateLaunchProfile StreamConfigurationCreate
createLaunchProfile_streamConfiguration = Lens.lens (\CreateLaunchProfile' {streamConfiguration} -> streamConfiguration) (\s@CreateLaunchProfile' {} a -> s {streamConfiguration = a} :: CreateLaunchProfile)

instance Core.AWSRequest CreateLaunchProfile where
  type
    AWSResponse CreateLaunchProfile =
      CreateLaunchProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLaunchProfileResponse'
            Prelude.<$> (x Core..?> "launchProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLaunchProfile

instance Prelude.NFData CreateLaunchProfile

instance Core.ToHeaders CreateLaunchProfile where
  toHeaders CreateLaunchProfile' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateLaunchProfile where
  toJSON CreateLaunchProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("ec2SubnetIds" Core..= ec2SubnetIds),
            Prelude.Just
              ("studioComponentIds" Core..= studioComponentIds),
            Prelude.Just
              ( "launchProfileProtocolVersions"
                  Core..= launchProfileProtocolVersions
              ),
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("streamConfiguration" Core..= streamConfiguration)
          ]
      )

instance Core.ToPath CreateLaunchProfile where
  toPath CreateLaunchProfile' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles"
      ]

instance Core.ToQuery CreateLaunchProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLaunchProfileResponse' smart constructor.
data CreateLaunchProfileResponse = CreateLaunchProfileResponse'
  { -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfile', 'createLaunchProfileResponse_launchProfile' - The launch profile.
--
-- 'httpStatus', 'createLaunchProfileResponse_httpStatus' - The response's http status code.
newCreateLaunchProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLaunchProfileResponse
newCreateLaunchProfileResponse pHttpStatus_ =
  CreateLaunchProfileResponse'
    { launchProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The launch profile.
createLaunchProfileResponse_launchProfile :: Lens.Lens' CreateLaunchProfileResponse (Prelude.Maybe LaunchProfile)
createLaunchProfileResponse_launchProfile = Lens.lens (\CreateLaunchProfileResponse' {launchProfile} -> launchProfile) (\s@CreateLaunchProfileResponse' {} a -> s {launchProfile = a} :: CreateLaunchProfileResponse)

-- | The response's http status code.
createLaunchProfileResponse_httpStatus :: Lens.Lens' CreateLaunchProfileResponse Prelude.Int
createLaunchProfileResponse_httpStatus = Lens.lens (\CreateLaunchProfileResponse' {httpStatus} -> httpStatus) (\s@CreateLaunchProfileResponse' {} a -> s {httpStatus = a} :: CreateLaunchProfileResponse)

instance Prelude.NFData CreateLaunchProfileResponse
