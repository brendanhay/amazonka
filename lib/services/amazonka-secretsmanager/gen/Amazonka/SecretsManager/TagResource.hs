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
-- Module      : Amazonka.SecretsManager.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more tags, each consisting of a key name and a value, to
-- the specified secret. Tags are part of the secret\'s overall metadata,
-- and are not associated with any specific version of the secret. This
-- operation only appends tags to the existing list of tags. To remove
-- tags, you must use UntagResource.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per secret—50
--
-- -   Maximum key length—127 Unicode characters in UTF-8
--
-- -   Maximum value length—255 Unicode characters in UTF-8
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use the @aws:@ prefix in your tag names or values because
--     Amazon Web Services reserves it for Amazon Web Services use. You
--     can\'t edit or delete tag names or values with this prefix. Tags
--     with this prefix do not count against your tags per secret limit.
--
-- -   If you use your tagging schema across multiple services and
--     resources, remember other services might have restrictions on
--     allowed characters. Generally allowed characters: letters, spaces,
--     and numbers representable in UTF-8, plus the following special
--     characters: + - = . _ : \/ \@.
--
-- If you use tags as part of your security strategy, then adding or
-- removing a tag can change permissions. If successfully completing this
-- operation would result in you losing your permissions for this secret,
-- then the operation is blocked and returns an Access Denied error.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:TagResource
--
-- __Related operations__
--
-- -   To remove one or more tags from the collection attached to a secret,
--     use UntagResource.
--
-- -   To view the list of tags attached to a secret, use DescribeSecret.
module Amazonka.SecretsManager.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_secretId,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The identifier for the secret that you want to attach tags to. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text,
    -- | The tags to attach to the secret. Each element in the list consists of a
    -- @Key@ and a @Value@.
    --
    -- This parameter to the API requires a JSON text string argument.
    --
    -- For storing multiple values, we recommend that you use a JSON text
    -- string argument and specify key\/value pairs. For more information, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
    -- in the Amazon Web Services CLI User Guide.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'tagResource_secretId' - The identifier for the secret that you want to attach tags to. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
--
-- 'tags', 'tagResource_tags' - The tags to attach to the secret. Each element in the list consists of a
-- @Key@ and a @Value@.
--
-- This parameter to the API requires a JSON text string argument.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
newTagResource ::
  -- | 'secretId'
  Prelude.Text ->
  TagResource
newTagResource pSecretId_ =
  TagResource'
    { secretId = pSecretId_,
      tags = Prelude.mempty
    }

-- | The identifier for the secret that you want to attach tags to. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
tagResource_secretId :: Lens.Lens' TagResource Prelude.Text
tagResource_secretId = Lens.lens (\TagResource' {secretId} -> secretId) (\s@TagResource' {} a -> s {secretId = a} :: TagResource)

-- | The tags to attach to the secret. Each element in the list consists of a
-- @Key@ and a @Value@.
--
-- This parameter to the API requires a JSON text string argument.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens.coerced

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource where
  hashWithSalt salt' TagResource' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf secretId `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("secretsmanager.TagResource" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TagResource where
  toJSON TagResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Core..= secretId),
            Prelude.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath TagResource where
  toPath = Prelude.const "/"

instance Core.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse where
  rnf _ = ()
