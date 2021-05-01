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
-- Module      : Network.AWS.SecretsManager.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified secret.
--
-- This operation is idempotent. If a requested tag is not attached to the
-- secret, no error is returned and the secret metadata is unchanged.
--
-- If you use tags as part of your security strategy, then removing a tag
-- can change permissions. If successfully completing this operation would
-- result in you losing your permissions for this secret, then the
-- operation is blocked and returns an Access Denied error.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:UntagResource
--
-- __Related operations__
--
-- -   To add one or more tags to the collection attached to a secret, use
--     TagResource.
--
-- -   To view the list of tags attached to a secret, use DescribeSecret.
module Network.AWS.SecretsManager.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_secretId,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The identifier for the secret that you want to remove tags from. You can
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
    -- | A list of tag key names to remove from the secret. You don\'t specify
    -- the value. Both the key and its associated value are removed.
    --
    -- This parameter to the API requires a JSON text string argument. For
    -- information on how to format a JSON parameter for the various command
    -- line tool environments, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /AWS CLI User Guide/.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'untagResource_secretId' - The identifier for the secret that you want to remove tags from. You can
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
-- 'tagKeys', 'untagResource_tagKeys' - A list of tag key names to remove from the secret. You don\'t specify
-- the value. Both the key and its associated value are removed.
--
-- This parameter to the API requires a JSON text string argument. For
-- information on how to format a JSON parameter for the various command
-- line tool environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/.
newUntagResource ::
  -- | 'secretId'
  Prelude.Text ->
  UntagResource
newUntagResource pSecretId_ =
  UntagResource'
    { secretId = pSecretId_,
      tagKeys = Prelude.mempty
    }

-- | The identifier for the secret that you want to remove tags from. You can
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
untagResource_secretId :: Lens.Lens' UntagResource Prelude.Text
untagResource_secretId = Lens.lens (\UntagResource' {secretId} -> secretId) (\s@UntagResource' {} a -> s {secretId = a} :: UntagResource)

-- | A list of tag key names to remove from the secret. You don\'t specify
-- the value. Both the key and its associated value are removed.
--
-- This parameter to the API requires a JSON text string argument. For
-- information on how to format a JSON parameter for the various command
-- line tool environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/.
untagResource_tagKeys :: Lens.Lens' UntagResource [Prelude.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource

instance Prelude.NFData UntagResource

instance Prelude.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "secretsmanager.UntagResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Prelude..= secretId),
            Prelude.Just ("TagKeys" Prelude..= tagKeys)
          ]
      )

instance Prelude.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Prelude.NFData UntagResourceResponse
