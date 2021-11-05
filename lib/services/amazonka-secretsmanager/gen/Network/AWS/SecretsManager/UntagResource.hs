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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The identifier for the secret that you want to remove tags from. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text,
    -- | A list of tag key names to remove from the secret. You don\'t specify
    -- the value. Both the key and its associated value are removed.
    --
    -- This parameter to the API requires a JSON text string argument.
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
-- 'secretId', 'untagResource_secretId' - The identifier for the secret that you want to remove tags from. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
--
-- 'tagKeys', 'untagResource_tagKeys' - A list of tag key names to remove from the secret. You don\'t specify
-- the value. Both the key and its associated value are removed.
--
-- This parameter to the API requires a JSON text string argument.
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

-- | The identifier for the secret that you want to remove tags from. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
untagResource_secretId :: Lens.Lens' UntagResource Prelude.Text
untagResource_secretId = Lens.lens (\UntagResource' {secretId} -> secretId) (\s@UntagResource' {} a -> s {secretId = a} :: UntagResource)

-- | A list of tag key names to remove from the secret. You don\'t specify
-- the value. Both the key and its associated value are removed.
--
-- This parameter to the API requires a JSON text string argument.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource

instance Prelude.NFData UntagResource

instance Core.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.UntagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Core..= secretId),
            Prelude.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Core.ToQuery UntagResource where
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

instance Prelude.NFData UntagResourceResponse
