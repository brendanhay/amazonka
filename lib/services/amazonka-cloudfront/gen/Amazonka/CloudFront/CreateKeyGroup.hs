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
-- Module      : Amazonka.CloudFront.CreateKeyGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudFront.CreateKeyGroup
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

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKeyGroup' smart constructor.
data CreateKeyGroup = CreateKeyGroup'
  { -- | A key group configuration.
    keyGroupConfig :: KeyGroupConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateKeyGroupResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKeyGroup where
  hashWithSalt _salt CreateKeyGroup' {..} =
    _salt `Prelude.hashWithSalt` keyGroupConfig

instance Prelude.NFData CreateKeyGroup where
  rnf CreateKeyGroup' {..} = Prelude.rnf keyGroupConfig

instance Data.ToElement CreateKeyGroup where
  toElement CreateKeyGroup' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      keyGroupConfig

instance Data.ToHeaders CreateKeyGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateKeyGroup where
  toPath = Prelude.const "/2020-05-31/key-group"

instance Data.ToQuery CreateKeyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKeyGroupResponse' smart constructor.
data CreateKeyGroupResponse = CreateKeyGroupResponse'
  { -- | The identifier for this version of the key group.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The key group that was just created.
    keyGroup :: Prelude.Maybe KeyGroup,
    -- | The URL of the key group.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateKeyGroupResponse
newCreateKeyGroupResponse pHttpStatus_ =
  CreateKeyGroupResponse'
    { eTag = Prelude.Nothing,
      keyGroup = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the key group.
createKeyGroupResponse_eTag :: Lens.Lens' CreateKeyGroupResponse (Prelude.Maybe Prelude.Text)
createKeyGroupResponse_eTag = Lens.lens (\CreateKeyGroupResponse' {eTag} -> eTag) (\s@CreateKeyGroupResponse' {} a -> s {eTag = a} :: CreateKeyGroupResponse)

-- | The key group that was just created.
createKeyGroupResponse_keyGroup :: Lens.Lens' CreateKeyGroupResponse (Prelude.Maybe KeyGroup)
createKeyGroupResponse_keyGroup = Lens.lens (\CreateKeyGroupResponse' {keyGroup} -> keyGroup) (\s@CreateKeyGroupResponse' {} a -> s {keyGroup = a} :: CreateKeyGroupResponse)

-- | The URL of the key group.
createKeyGroupResponse_location :: Lens.Lens' CreateKeyGroupResponse (Prelude.Maybe Prelude.Text)
createKeyGroupResponse_location = Lens.lens (\CreateKeyGroupResponse' {location} -> location) (\s@CreateKeyGroupResponse' {} a -> s {location = a} :: CreateKeyGroupResponse)

-- | The response's http status code.
createKeyGroupResponse_httpStatus :: Lens.Lens' CreateKeyGroupResponse Prelude.Int
createKeyGroupResponse_httpStatus = Lens.lens (\CreateKeyGroupResponse' {httpStatus} -> httpStatus) (\s@CreateKeyGroupResponse' {} a -> s {httpStatus = a} :: CreateKeyGroupResponse)

instance Prelude.NFData CreateKeyGroupResponse where
  rnf CreateKeyGroupResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf keyGroup
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
