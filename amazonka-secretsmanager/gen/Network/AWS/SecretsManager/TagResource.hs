{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SecretsManager.TagResource
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
-- -   Do not use the @aws:@ prefix in your tag names or values because AWS
--     reserves it for AWS use. You can\'t edit or delete tag names or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per secret limit.
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
module Network.AWS.SecretsManager.TagResource
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The identifier for the secret that you want to attach tags to. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
    secretId :: Prelude.Text,
    -- | The tags to attach to the secret. Each element in the list consists of a
    -- @Key@ and a @Value@.
    --
    -- This parameter to the API requires a JSON text string argument. For
    -- information on how to format a JSON parameter for the various command
    -- line tool environments, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /AWS CLI User Guide/. For the AWS CLI, you can also use the
    -- syntax:
    -- @--Tags Key=\"Key1\",Value=\"Value1\" Key=\"Key2\",Value=\"Value2\"[,…]@
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
--
-- 'tags', 'tagResource_tags' - The tags to attach to the secret. Each element in the list consists of a
-- @Key@ and a @Value@.
--
-- This parameter to the API requires a JSON text string argument. For
-- information on how to format a JSON parameter for the various command
-- line tool environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/. For the AWS CLI, you can also use the
-- syntax:
-- @--Tags Key=\"Key1\",Value=\"Value1\" Key=\"Key2\",Value=\"Value2\"[,…]@
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
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
tagResource_secretId :: Lens.Lens' TagResource Prelude.Text
tagResource_secretId = Lens.lens (\TagResource' {secretId} -> secretId) (\s@TagResource' {} a -> s {secretId = a} :: TagResource)

-- | The tags to attach to the secret. Each element in the list consists of a
-- @Key@ and a @Value@.
--
-- This parameter to the API requires a JSON text string argument. For
-- information on how to format a JSON parameter for the various command
-- line tool environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/. For the AWS CLI, you can also use the
-- syntax:
-- @--Tags Key=\"Key1\",Value=\"Value1\" Key=\"Key2\",Value=\"Value2\"[,…]@
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull TagResourceResponse'

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Prelude.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("secretsmanager.TagResource" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Prelude..= secretId),
            Prelude.Just ("Tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath TagResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagResourceResponse ::
  TagResourceResponse
newTagResourceResponse = TagResourceResponse'

instance Prelude.NFData TagResourceResponse
