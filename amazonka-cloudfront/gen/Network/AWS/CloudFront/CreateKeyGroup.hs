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
-- Module      : Network.AWS.CloudFront.CreateKeyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a key group that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies>.
--
-- To create a key group, you must specify at least one public key for the
-- key group. After you create a key group, you can reference it from one
-- or more cache behaviors. When you reference a key group in a cache
-- behavior, CloudFront requires signed URLs or signed cookies for all
-- requests that match the cache behavior. The URLs or cookies must be
-- signed with a private key whose corresponding public key is in the key
-- group. The signed URL or cookie contains information about which public
-- key CloudFront should use to verify the signature. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateKeyGroup
  ( -- * Creating a Request
    CreateKeyGroup (..),
    newCreateKeyGroup,

    -- * Request Lenses
    createKeyGroup_keyGroupConfig,

    -- * Destructuring the Response
    CreateKeyGroupResponse (..),
    newCreateKeyGroupResponse,

    -- * Response Lenses
    createKeyGroupResponse_eTag,
    createKeyGroupResponse_keyGroup,
    createKeyGroupResponse_location,
    createKeyGroupResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateKeyGroup' smart constructor.
data CreateKeyGroup = CreateKeyGroup'
  { -- | A key group configuration.
    keyGroupConfig :: KeyGroupConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyGroupConfig', 'createKeyGroup_keyGroupConfig' - A key group configuration.
newCreateKeyGroup ::
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  CreateKeyGroup
newCreateKeyGroup pKeyGroupConfig_ =
  CreateKeyGroup' {keyGroupConfig = pKeyGroupConfig_}

-- | A key group configuration.
createKeyGroup_keyGroupConfig :: Lens.Lens' CreateKeyGroup KeyGroupConfig
createKeyGroup_keyGroupConfig = Lens.lens (\CreateKeyGroup' {keyGroupConfig} -> keyGroupConfig) (\s@CreateKeyGroup' {} a -> s {keyGroupConfig = a} :: CreateKeyGroup)

instance Core.AWSRequest CreateKeyGroup where
  type
    AWSResponse CreateKeyGroup =
      CreateKeyGroupResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateKeyGroupResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateKeyGroup

instance Core.NFData CreateKeyGroup

instance Core.ToElement CreateKeyGroup where
  toElement CreateKeyGroup' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      keyGroupConfig

instance Core.ToHeaders CreateKeyGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateKeyGroup where
  toPath = Core.const "/2020-05-31/key-group"

instance Core.ToQuery CreateKeyGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateKeyGroupResponse' smart constructor.
data CreateKeyGroupResponse = CreateKeyGroupResponse'
  { -- | The identifier for this version of the key group.
    eTag :: Core.Maybe Core.Text,
    -- | The key group that was just created.
    keyGroup :: Core.Maybe KeyGroup,
    -- | The URL of the key group.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createKeyGroupResponse_eTag' - The identifier for this version of the key group.
--
-- 'keyGroup', 'createKeyGroupResponse_keyGroup' - The key group that was just created.
--
-- 'location', 'createKeyGroupResponse_location' - The URL of the key group.
--
-- 'httpStatus', 'createKeyGroupResponse_httpStatus' - The response's http status code.
newCreateKeyGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateKeyGroupResponse
newCreateKeyGroupResponse pHttpStatus_ =
  CreateKeyGroupResponse'
    { eTag = Core.Nothing,
      keyGroup = Core.Nothing,
      location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the key group.
createKeyGroupResponse_eTag :: Lens.Lens' CreateKeyGroupResponse (Core.Maybe Core.Text)
createKeyGroupResponse_eTag = Lens.lens (\CreateKeyGroupResponse' {eTag} -> eTag) (\s@CreateKeyGroupResponse' {} a -> s {eTag = a} :: CreateKeyGroupResponse)

-- | The key group that was just created.
createKeyGroupResponse_keyGroup :: Lens.Lens' CreateKeyGroupResponse (Core.Maybe KeyGroup)
createKeyGroupResponse_keyGroup = Lens.lens (\CreateKeyGroupResponse' {keyGroup} -> keyGroup) (\s@CreateKeyGroupResponse' {} a -> s {keyGroup = a} :: CreateKeyGroupResponse)

-- | The URL of the key group.
createKeyGroupResponse_location :: Lens.Lens' CreateKeyGroupResponse (Core.Maybe Core.Text)
createKeyGroupResponse_location = Lens.lens (\CreateKeyGroupResponse' {location} -> location) (\s@CreateKeyGroupResponse' {} a -> s {location = a} :: CreateKeyGroupResponse)

-- | The response's http status code.
createKeyGroupResponse_httpStatus :: Lens.Lens' CreateKeyGroupResponse Core.Int
createKeyGroupResponse_httpStatus = Lens.lens (\CreateKeyGroupResponse' {httpStatus} -> httpStatus) (\s@CreateKeyGroupResponse' {} a -> s {httpStatus = a} :: CreateKeyGroupResponse)

instance Core.NFData CreateKeyGroupResponse
