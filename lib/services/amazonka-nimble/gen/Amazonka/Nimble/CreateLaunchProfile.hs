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
-- Module      : Amazonka.Nimble.CreateLaunchProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a launch profile.
module Amazonka.Nimble.CreateLaunchProfile
  ( -- * Creating a Request
    CreateLaunchProfile (..),
    newCreateLaunchProfile,

    -- * Request Lenses
    createLaunchProfile_clientToken,
    createLaunchProfile_description,
    createLaunchProfile_tags,
    createLaunchProfile_ec2SubnetIds,
    createLaunchProfile_launchProfileProtocolVersions,
    createLaunchProfile_name,
    createLaunchProfile_streamConfiguration,
    createLaunchProfile_studioComponentIds,
    createLaunchProfile_studioId,

    -- * Destructuring the Response
    CreateLaunchProfileResponse (..),
    newCreateLaunchProfileResponse,

    -- * Response Lenses
    createLaunchProfileResponse_launchProfile,
    createLaunchProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchProfile' smart constructor.
data CreateLaunchProfile = CreateLaunchProfile'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A collection of labels, in the form of key-value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the IDs of the EC2 subnets where streaming sessions will be
    -- accessible from. These subnets must support the specified instance
    -- types.
    ec2SubnetIds :: [Prelude.Text],
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersions :: [Prelude.Text],
    -- | The name for the launch profile.
    name :: Data.Sensitive Prelude.Text,
    -- | A configuration for a streaming session.
    streamConfiguration :: StreamConfigurationCreate,
    -- | Unique identifiers for a collection of studio components that can be
    -- used with this launch profile.
    studioComponentIds :: Prelude.NonEmpty Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createLaunchProfile_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'description', 'createLaunchProfile_description' - The description.
--
-- 'tags', 'createLaunchProfile_tags' - A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
--
-- 'ec2SubnetIds', 'createLaunchProfile_ec2SubnetIds' - Specifies the IDs of the EC2 subnets where streaming sessions will be
-- accessible from. These subnets must support the specified instance
-- types.
--
-- 'launchProfileProtocolVersions', 'createLaunchProfile_launchProfileProtocolVersions' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'name', 'createLaunchProfile_name' - The name for the launch profile.
--
-- 'streamConfiguration', 'createLaunchProfile_streamConfiguration' - A configuration for a streaming session.
--
-- 'studioComponentIds', 'createLaunchProfile_studioComponentIds' - Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
--
-- 'studioId', 'createLaunchProfile_studioId' - The studio ID.
newCreateLaunchProfile ::
  -- | 'name'
  Prelude.Text ->
  -- | 'streamConfiguration'
  StreamConfigurationCreate ->
  -- | 'studioComponentIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  CreateLaunchProfile
newCreateLaunchProfile
  pName_
  pStreamConfiguration_
  pStudioComponentIds_
  pStudioId_ =
    CreateLaunchProfile'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        ec2SubnetIds = Prelude.mempty,
        launchProfileProtocolVersions = Prelude.mempty,
        name = Data._Sensitive Lens.# pName_,
        streamConfiguration = pStreamConfiguration_,
        studioComponentIds =
          Lens.coerced Lens.# pStudioComponentIds_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
createLaunchProfile_clientToken :: Lens.Lens' CreateLaunchProfile (Prelude.Maybe Prelude.Text)
createLaunchProfile_clientToken = Lens.lens (\CreateLaunchProfile' {clientToken} -> clientToken) (\s@CreateLaunchProfile' {} a -> s {clientToken = a} :: CreateLaunchProfile)

-- | The description.
createLaunchProfile_description :: Lens.Lens' CreateLaunchProfile (Prelude.Maybe Prelude.Text)
createLaunchProfile_description = Lens.lens (\CreateLaunchProfile' {description} -> description) (\s@CreateLaunchProfile' {} a -> s {description = a} :: CreateLaunchProfile) Prelude.. Lens.mapping Data._Sensitive

-- | A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
createLaunchProfile_tags :: Lens.Lens' CreateLaunchProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLaunchProfile_tags = Lens.lens (\CreateLaunchProfile' {tags} -> tags) (\s@CreateLaunchProfile' {} a -> s {tags = a} :: CreateLaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the IDs of the EC2 subnets where streaming sessions will be
-- accessible from. These subnets must support the specified instance
-- types.
createLaunchProfile_ec2SubnetIds :: Lens.Lens' CreateLaunchProfile [Prelude.Text]
createLaunchProfile_ec2SubnetIds = Lens.lens (\CreateLaunchProfile' {ec2SubnetIds} -> ec2SubnetIds) (\s@CreateLaunchProfile' {} a -> s {ec2SubnetIds = a} :: CreateLaunchProfile) Prelude.. Lens.coerced

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
createLaunchProfile_launchProfileProtocolVersions :: Lens.Lens' CreateLaunchProfile [Prelude.Text]
createLaunchProfile_launchProfileProtocolVersions = Lens.lens (\CreateLaunchProfile' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@CreateLaunchProfile' {} a -> s {launchProfileProtocolVersions = a} :: CreateLaunchProfile) Prelude.. Lens.coerced

-- | The name for the launch profile.
createLaunchProfile_name :: Lens.Lens' CreateLaunchProfile Prelude.Text
createLaunchProfile_name = Lens.lens (\CreateLaunchProfile' {name} -> name) (\s@CreateLaunchProfile' {} a -> s {name = a} :: CreateLaunchProfile) Prelude.. Data._Sensitive

-- | A configuration for a streaming session.
createLaunchProfile_streamConfiguration :: Lens.Lens' CreateLaunchProfile StreamConfigurationCreate
createLaunchProfile_streamConfiguration = Lens.lens (\CreateLaunchProfile' {streamConfiguration} -> streamConfiguration) (\s@CreateLaunchProfile' {} a -> s {streamConfiguration = a} :: CreateLaunchProfile)

-- | Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
createLaunchProfile_studioComponentIds :: Lens.Lens' CreateLaunchProfile (Prelude.NonEmpty Prelude.Text)
createLaunchProfile_studioComponentIds = Lens.lens (\CreateLaunchProfile' {studioComponentIds} -> studioComponentIds) (\s@CreateLaunchProfile' {} a -> s {studioComponentIds = a} :: CreateLaunchProfile) Prelude.. Lens.coerced

-- | The studio ID.
createLaunchProfile_studioId :: Lens.Lens' CreateLaunchProfile Prelude.Text
createLaunchProfile_studioId = Lens.lens (\CreateLaunchProfile' {studioId} -> studioId) (\s@CreateLaunchProfile' {} a -> s {studioId = a} :: CreateLaunchProfile)

instance Core.AWSRequest CreateLaunchProfile where
  type
    AWSResponse CreateLaunchProfile =
      CreateLaunchProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLaunchProfileResponse'
            Prelude.<$> (x Data..?> "launchProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLaunchProfile where
  hashWithSalt _salt CreateLaunchProfile' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ec2SubnetIds
      `Prelude.hashWithSalt` launchProfileProtocolVersions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` streamConfiguration
      `Prelude.hashWithSalt` studioComponentIds
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData CreateLaunchProfile where
  rnf CreateLaunchProfile' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ec2SubnetIds
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf streamConfiguration
      `Prelude.seq` Prelude.rnf studioComponentIds
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders CreateLaunchProfile where
  toHeaders CreateLaunchProfile' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateLaunchProfile where
  toJSON CreateLaunchProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ec2SubnetIds" Data..= ec2SubnetIds),
            Prelude.Just
              ( "launchProfileProtocolVersions"
                  Data..= launchProfileProtocolVersions
              ),
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("streamConfiguration" Data..= streamConfiguration),
            Prelude.Just
              ("studioComponentIds" Data..= studioComponentIds)
          ]
      )

instance Data.ToPath CreateLaunchProfile where
  toPath CreateLaunchProfile' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/launch-profiles"
      ]

instance Data.ToQuery CreateLaunchProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLaunchProfileResponse' smart constructor.
data CreateLaunchProfileResponse = CreateLaunchProfileResponse'
  { -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateLaunchProfileResponse where
  rnf CreateLaunchProfileResponse' {..} =
    Prelude.rnf launchProfile
      `Prelude.seq` Prelude.rnf httpStatus
