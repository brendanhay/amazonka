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
-- Module      : Amazonka.SecretsManager.RestoreSecret
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
module Amazonka.SecretsManager.RestoreSecret
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newRestoreSecret' smart constructor.
data RestoreSecret = RestoreSecret'
  { -- | Specifies the secret that you want to restore from a previously
    -- scheduled deletion. You can specify either the Amazon Resource Name
    -- (ARN) or the friendly name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
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
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
restoreSecret_secretId :: Lens.Lens' RestoreSecret Prelude.Text
restoreSecret_secretId = Lens.lens (\RestoreSecret' {secretId} -> secretId) (\s@RestoreSecret' {} a -> s {secretId = a} :: RestoreSecret)

instance Core.AWSRequest RestoreSecret where
  type
    AWSResponse RestoreSecret =
      RestoreSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreSecretResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreSecret

instance Prelude.NFData RestoreSecret

instance Core.ToHeaders RestoreSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.RestoreSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RestoreSecret where
  toJSON RestoreSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Core..= secretId)]
      )

instance Core.ToPath RestoreSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreSecret where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
