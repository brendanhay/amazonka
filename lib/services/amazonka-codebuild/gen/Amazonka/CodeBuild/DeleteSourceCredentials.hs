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
-- Module      : Amazonka.CodeBuild.DeleteSourceCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source
-- credentials.
module Amazonka.CodeBuild.DeleteSourceCredentials
  ( -- * Creating a Request
    DeleteSourceCredentials (..),
    newDeleteSourceCredentials,

    -- * Request Lenses
    deleteSourceCredentials_arn,

    -- * Destructuring the Response
    DeleteSourceCredentialsResponse (..),
    newDeleteSourceCredentialsResponse,

    -- * Response Lenses
    deleteSourceCredentialsResponse_arn,
    deleteSourceCredentialsResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSourceCredentials' smart constructor.
data DeleteSourceCredentials = DeleteSourceCredentials'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSourceCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteSourceCredentials_arn' - The Amazon Resource Name (ARN) of the token.
newDeleteSourceCredentials ::
  -- | 'arn'
  Prelude.Text ->
  DeleteSourceCredentials
newDeleteSourceCredentials pArn_ =
  DeleteSourceCredentials' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the token.
deleteSourceCredentials_arn :: Lens.Lens' DeleteSourceCredentials Prelude.Text
deleteSourceCredentials_arn = Lens.lens (\DeleteSourceCredentials' {arn} -> arn) (\s@DeleteSourceCredentials' {} a -> s {arn = a} :: DeleteSourceCredentials)

instance Core.AWSRequest DeleteSourceCredentials where
  type
    AWSResponse DeleteSourceCredentials =
      DeleteSourceCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSourceCredentialsResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSourceCredentials where
  hashWithSalt _salt DeleteSourceCredentials' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteSourceCredentials where
  rnf DeleteSourceCredentials' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteSourceCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.DeleteSourceCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSourceCredentials where
  toJSON DeleteSourceCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteSourceCredentials where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSourceCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSourceCredentialsResponse' smart constructor.
data DeleteSourceCredentialsResponse = DeleteSourceCredentialsResponse'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSourceCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteSourceCredentialsResponse_arn' - The Amazon Resource Name (ARN) of the token.
--
-- 'httpStatus', 'deleteSourceCredentialsResponse_httpStatus' - The response's http status code.
newDeleteSourceCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSourceCredentialsResponse
newDeleteSourceCredentialsResponse pHttpStatus_ =
  DeleteSourceCredentialsResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the token.
deleteSourceCredentialsResponse_arn :: Lens.Lens' DeleteSourceCredentialsResponse (Prelude.Maybe Prelude.Text)
deleteSourceCredentialsResponse_arn = Lens.lens (\DeleteSourceCredentialsResponse' {arn} -> arn) (\s@DeleteSourceCredentialsResponse' {} a -> s {arn = a} :: DeleteSourceCredentialsResponse)

-- | The response's http status code.
deleteSourceCredentialsResponse_httpStatus :: Lens.Lens' DeleteSourceCredentialsResponse Prelude.Int
deleteSourceCredentialsResponse_httpStatus = Lens.lens (\DeleteSourceCredentialsResponse' {httpStatus} -> httpStatus) (\s@DeleteSourceCredentialsResponse' {} a -> s {httpStatus = a} :: DeleteSourceCredentialsResponse)

instance
  Prelude.NFData
    DeleteSourceCredentialsResponse
  where
  rnf DeleteSourceCredentialsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
