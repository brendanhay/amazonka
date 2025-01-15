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
-- Module      : Amazonka.SecretsManager.UntagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes specific tags from a secret.
--
-- This operation is idempotent. If a requested tag is not attached to the
-- secret, no error is returned and the secret metadata is unchanged.
--
-- If you use tags as part of your security strategy, then removing a tag
-- can change permissions. If successfully completing this operation would
-- result in you losing your permissions for this secret, then the
-- operation is blocked and returns an Access Denied error.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:UntagResource@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.UntagResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The ARN or name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text,
    -- | A list of tag key names to remove from the secret. You don\'t specify
    -- the value. Both the key and its associated value are removed.
    --
    -- This parameter requires a JSON text string argument.
    --
    -- For storing multiple values, we recommend that you use a JSON text
    -- string argument and specify key\/value pairs. For more information, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
    -- in the Amazon Web Services CLI User Guide.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'untagResource_secretId' - The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
--
-- 'tagKeys', 'untagResource_tagKeys' - A list of tag key names to remove from the secret. You don\'t specify
-- the value. Both the key and its associated value are removed.
--
-- This parameter requires a JSON text string argument.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
newUntagResource ::
  -- | 'secretId'
  Prelude.Text ->
  UntagResource
newUntagResource pSecretId_ =
  UntagResource'
    { secretId = pSecretId_,
      tagKeys = Prelude.mempty
    }

-- | The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
untagResource_secretId :: Lens.Lens' UntagResource Prelude.Text
untagResource_secretId = Lens.lens (\UntagResource' {secretId} -> secretId) (\s@UntagResource' {} a -> s {secretId = a} :: UntagResource)

-- | A list of tag key names to remove from the secret. You don\'t specify
-- the value. Both the key and its associated value are removed.
--
-- This parameter requires a JSON text string argument.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
untagResource_tagKeys :: Lens.Lens' UntagResource [Prelude.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Prelude.. Lens.coerced

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource where
  hashWithSalt _salt UntagResource' {..} =
    _salt
      `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf secretId `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.UntagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Data..= secretId),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Prelude.NFData UntagResourceResponse where
  rnf _ = ()
