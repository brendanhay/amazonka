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
-- Module      : Network.AWS.SecretsManager.RestoreSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the scheduled deletion of a secret by removing the @DeletedDate@
-- time stamp. This makes the secret accessible to query once again.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:RestoreSecret
--
-- __Related operations__
--
-- -   To delete a secret, use DeleteSecret.
module Network.AWS.SecretsManager.RestoreSecret
  ( -- * Creating a Request
    RestoreSecret (..),
    newRestoreSecret,

    -- * Request Lenses
    restoreSecret_secretId,

    -- * Destructuring the Response
    RestoreSecretResponse (..),
    newRestoreSecretResponse,

    -- * Response Lenses
    restoreSecretResponse_arn,
    restoreSecretResponse_name,
    restoreSecretResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newRestoreSecret' smart constructor.
data RestoreSecret = RestoreSecret'
  { -- | Specifies the secret that you want to restore from a previously
    -- scheduled deletion. You can specify either the Amazon Resource Name
    -- (ARN) or the friendly name of the secret.
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
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'restoreSecret_secretId' - Specifies the secret that you want to restore from a previously
-- scheduled deletion. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
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
newRestoreSecret ::
  -- | 'secretId'
  Prelude.Text ->
  RestoreSecret
newRestoreSecret pSecretId_ =
  RestoreSecret' {secretId = pSecretId_}

-- | Specifies the secret that you want to restore from a previously
-- scheduled deletion. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
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
restoreSecret_secretId :: Lens.Lens' RestoreSecret Prelude.Text
restoreSecret_secretId = Lens.lens (\RestoreSecret' {secretId} -> secretId) (\s@RestoreSecret' {} a -> s {secretId = a} :: RestoreSecret)

instance Prelude.AWSRequest RestoreSecret where
  type Rs RestoreSecret = RestoreSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreSecretResponse'
            Prelude.<$> (x Prelude..?> "ARN")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreSecret

instance Prelude.NFData RestoreSecret

instance Prelude.ToHeaders RestoreSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "secretsmanager.RestoreSecret" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RestoreSecret where
  toJSON RestoreSecret' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Prelude..= secretId)]
      )

instance Prelude.ToPath RestoreSecret where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RestoreSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreSecretResponse' smart constructor.
data RestoreSecretResponse = RestoreSecretResponse'
  { -- | The ARN of the secret that was restored.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret that was restored.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestoreSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'restoreSecretResponse_arn' - The ARN of the secret that was restored.
--
-- 'name', 'restoreSecretResponse_name' - The friendly name of the secret that was restored.
--
-- 'httpStatus', 'restoreSecretResponse_httpStatus' - The response's http status code.
newRestoreSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreSecretResponse
newRestoreSecretResponse pHttpStatus_ =
  RestoreSecretResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that was restored.
restoreSecretResponse_arn :: Lens.Lens' RestoreSecretResponse (Prelude.Maybe Prelude.Text)
restoreSecretResponse_arn = Lens.lens (\RestoreSecretResponse' {arn} -> arn) (\s@RestoreSecretResponse' {} a -> s {arn = a} :: RestoreSecretResponse)

-- | The friendly name of the secret that was restored.
restoreSecretResponse_name :: Lens.Lens' RestoreSecretResponse (Prelude.Maybe Prelude.Text)
restoreSecretResponse_name = Lens.lens (\RestoreSecretResponse' {name} -> name) (\s@RestoreSecretResponse' {} a -> s {name = a} :: RestoreSecretResponse)

-- | The response's http status code.
restoreSecretResponse_httpStatus :: Lens.Lens' RestoreSecretResponse Prelude.Int
restoreSecretResponse_httpStatus = Lens.lens (\RestoreSecretResponse' {httpStatus} -> httpStatus) (\s@RestoreSecretResponse' {} a -> s {httpStatus = a} :: RestoreSecretResponse)

instance Prelude.NFData RestoreSecretResponse
