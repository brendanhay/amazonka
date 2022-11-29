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
-- Module      : Amazonka.Connect.CreateSecurityProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Creates a security profile.
module Amazonka.Connect.CreateSecurityProfile
  ( -- * Creating a Request
    CreateSecurityProfile (..),
    newCreateSecurityProfile,

    -- * Request Lenses
    createSecurityProfile_tags,
    createSecurityProfile_allowedAccessControlTags,
    createSecurityProfile_permissions,
    createSecurityProfile_description,
    createSecurityProfile_tagRestrictedResources,
    createSecurityProfile_securityProfileName,
    createSecurityProfile_instanceId,

    -- * Destructuring the Response
    CreateSecurityProfileResponse (..),
    newCreateSecurityProfileResponse,

    -- * Response Lenses
    createSecurityProfileResponse_securityProfileId,
    createSecurityProfileResponse_securityProfileArn,
    createSecurityProfileResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSecurityProfile' smart constructor.
data CreateSecurityProfile = CreateSecurityProfile'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The list of tags that a security profile uses to restrict access to
    -- resources in Amazon Connect.
    allowedAccessControlTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Permissions assigned to the security profile. For a list of valid
    -- permissions, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-list.html List of security profile permissions>.
    permissions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the security profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of resources that a security profile applies tag restrictions
    -- to in Amazon Connect.
    tagRestrictedResources :: Prelude.Maybe [Prelude.Text],
    -- | The name of the security profile.
    securityProfileName :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSecurityProfile_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'allowedAccessControlTags', 'createSecurityProfile_allowedAccessControlTags' - The list of tags that a security profile uses to restrict access to
-- resources in Amazon Connect.
--
-- 'permissions', 'createSecurityProfile_permissions' - Permissions assigned to the security profile. For a list of valid
-- permissions, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-list.html List of security profile permissions>.
--
-- 'description', 'createSecurityProfile_description' - The description of the security profile.
--
-- 'tagRestrictedResources', 'createSecurityProfile_tagRestrictedResources' - The list of resources that a security profile applies tag restrictions
-- to in Amazon Connect.
--
-- 'securityProfileName', 'createSecurityProfile_securityProfileName' - The name of the security profile.
--
-- 'instanceId', 'createSecurityProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newCreateSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  CreateSecurityProfile
newCreateSecurityProfile
  pSecurityProfileName_
  pInstanceId_ =
    CreateSecurityProfile'
      { tags = Prelude.Nothing,
        allowedAccessControlTags = Prelude.Nothing,
        permissions = Prelude.Nothing,
        description = Prelude.Nothing,
        tagRestrictedResources = Prelude.Nothing,
        securityProfileName = pSecurityProfileName_,
        instanceId = pInstanceId_
      }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createSecurityProfile_tags :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSecurityProfile_tags = Lens.lens (\CreateSecurityProfile' {tags} -> tags) (\s@CreateSecurityProfile' {} a -> s {tags = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The list of tags that a security profile uses to restrict access to
-- resources in Amazon Connect.
createSecurityProfile_allowedAccessControlTags :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSecurityProfile_allowedAccessControlTags = Lens.lens (\CreateSecurityProfile' {allowedAccessControlTags} -> allowedAccessControlTags) (\s@CreateSecurityProfile' {} a -> s {allowedAccessControlTags = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | Permissions assigned to the security profile. For a list of valid
-- permissions, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-list.html List of security profile permissions>.
createSecurityProfile_permissions :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe [Prelude.Text])
createSecurityProfile_permissions = Lens.lens (\CreateSecurityProfile' {permissions} -> permissions) (\s@CreateSecurityProfile' {} a -> s {permissions = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The description of the security profile.
createSecurityProfile_description :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe Prelude.Text)
createSecurityProfile_description = Lens.lens (\CreateSecurityProfile' {description} -> description) (\s@CreateSecurityProfile' {} a -> s {description = a} :: CreateSecurityProfile)

-- | The list of resources that a security profile applies tag restrictions
-- to in Amazon Connect.
createSecurityProfile_tagRestrictedResources :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe [Prelude.Text])
createSecurityProfile_tagRestrictedResources = Lens.lens (\CreateSecurityProfile' {tagRestrictedResources} -> tagRestrictedResources) (\s@CreateSecurityProfile' {} a -> s {tagRestrictedResources = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the security profile.
createSecurityProfile_securityProfileName :: Lens.Lens' CreateSecurityProfile Prelude.Text
createSecurityProfile_securityProfileName = Lens.lens (\CreateSecurityProfile' {securityProfileName} -> securityProfileName) (\s@CreateSecurityProfile' {} a -> s {securityProfileName = a} :: CreateSecurityProfile)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createSecurityProfile_instanceId :: Lens.Lens' CreateSecurityProfile Prelude.Text
createSecurityProfile_instanceId = Lens.lens (\CreateSecurityProfile' {instanceId} -> instanceId) (\s@CreateSecurityProfile' {} a -> s {instanceId = a} :: CreateSecurityProfile)

instance Core.AWSRequest CreateSecurityProfile where
  type
    AWSResponse CreateSecurityProfile =
      CreateSecurityProfileResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityProfileResponse'
            Prelude.<$> (x Core..?> "SecurityProfileId")
            Prelude.<*> (x Core..?> "SecurityProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecurityProfile where
  hashWithSalt _salt CreateSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` allowedAccessControlTags
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tagRestrictedResources
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData CreateSecurityProfile where
  rnf CreateSecurityProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf allowedAccessControlTags
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tagRestrictedResources
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders CreateSecurityProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSecurityProfile where
  toJSON CreateSecurityProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("AllowedAccessControlTags" Core..=)
              Prelude.<$> allowedAccessControlTags,
            ("Permissions" Core..=) Prelude.<$> permissions,
            ("Description" Core..=) Prelude.<$> description,
            ("TagRestrictedResources" Core..=)
              Prelude.<$> tagRestrictedResources,
            Prelude.Just
              ("SecurityProfileName" Core..= securityProfileName)
          ]
      )

instance Core.ToPath CreateSecurityProfile where
  toPath CreateSecurityProfile' {..} =
    Prelude.mconcat
      ["/security-profiles/", Core.toBS instanceId]

instance Core.ToQuery CreateSecurityProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecurityProfileResponse' smart constructor.
data CreateSecurityProfileResponse = CreateSecurityProfileResponse'
  { -- | The identifier for the security profle.
    securityProfileId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the security profile.
    securityProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileId', 'createSecurityProfileResponse_securityProfileId' - The identifier for the security profle.
--
-- 'securityProfileArn', 'createSecurityProfileResponse_securityProfileArn' - The Amazon Resource Name (ARN) for the security profile.
--
-- 'httpStatus', 'createSecurityProfileResponse_httpStatus' - The response's http status code.
newCreateSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSecurityProfileResponse
newCreateSecurityProfileResponse pHttpStatus_ =
  CreateSecurityProfileResponse'
    { securityProfileId =
        Prelude.Nothing,
      securityProfileArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the security profle.
createSecurityProfileResponse_securityProfileId :: Lens.Lens' CreateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
createSecurityProfileResponse_securityProfileId = Lens.lens (\CreateSecurityProfileResponse' {securityProfileId} -> securityProfileId) (\s@CreateSecurityProfileResponse' {} a -> s {securityProfileId = a} :: CreateSecurityProfileResponse)

-- | The Amazon Resource Name (ARN) for the security profile.
createSecurityProfileResponse_securityProfileArn :: Lens.Lens' CreateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
createSecurityProfileResponse_securityProfileArn = Lens.lens (\CreateSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@CreateSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: CreateSecurityProfileResponse)

-- | The response's http status code.
createSecurityProfileResponse_httpStatus :: Lens.Lens' CreateSecurityProfileResponse Prelude.Int
createSecurityProfileResponse_httpStatus = Lens.lens (\CreateSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityProfileResponse' {} a -> s {httpStatus = a} :: CreateSecurityProfileResponse)

instance Prelude.NFData CreateSecurityProfileResponse where
  rnf CreateSecurityProfileResponse' {..} =
    Prelude.rnf securityProfileId
      `Prelude.seq` Prelude.rnf securityProfileArn
      `Prelude.seq` Prelude.rnf httpStatus
