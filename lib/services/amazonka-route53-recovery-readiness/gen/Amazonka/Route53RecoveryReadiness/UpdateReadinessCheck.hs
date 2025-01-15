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
-- Module      : Amazonka.Route53RecoveryReadiness.UpdateReadinessCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a readiness check.
module Amazonka.Route53RecoveryReadiness.UpdateReadinessCheck
  ( -- * Creating a Request
    UpdateReadinessCheck (..),
    newUpdateReadinessCheck,

    -- * Request Lenses
    updateReadinessCheck_readinessCheckName,
    updateReadinessCheck_resourceSetName,

    -- * Destructuring the Response
    UpdateReadinessCheckResponse (..),
    newUpdateReadinessCheckResponse,

    -- * Response Lenses
    updateReadinessCheckResponse_readinessCheckArn,
    updateReadinessCheckResponse_readinessCheckName,
    updateReadinessCheckResponse_resourceSet,
    updateReadinessCheckResponse_tags,
    updateReadinessCheckResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | Name of a readiness check to describe.
--
-- /See:/ 'newUpdateReadinessCheck' smart constructor.
data UpdateReadinessCheck = UpdateReadinessCheck'
  { -- | Name of a readiness check.
    readinessCheckName :: Prelude.Text,
    -- | The name of the resource set to be checked.
    resourceSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReadinessCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessCheckName', 'updateReadinessCheck_readinessCheckName' - Name of a readiness check.
--
-- 'resourceSetName', 'updateReadinessCheck_resourceSetName' - The name of the resource set to be checked.
newUpdateReadinessCheck ::
  -- | 'readinessCheckName'
  Prelude.Text ->
  -- | 'resourceSetName'
  Prelude.Text ->
  UpdateReadinessCheck
newUpdateReadinessCheck
  pReadinessCheckName_
  pResourceSetName_ =
    UpdateReadinessCheck'
      { readinessCheckName =
          pReadinessCheckName_,
        resourceSetName = pResourceSetName_
      }

-- | Name of a readiness check.
updateReadinessCheck_readinessCheckName :: Lens.Lens' UpdateReadinessCheck Prelude.Text
updateReadinessCheck_readinessCheckName = Lens.lens (\UpdateReadinessCheck' {readinessCheckName} -> readinessCheckName) (\s@UpdateReadinessCheck' {} a -> s {readinessCheckName = a} :: UpdateReadinessCheck)

-- | The name of the resource set to be checked.
updateReadinessCheck_resourceSetName :: Lens.Lens' UpdateReadinessCheck Prelude.Text
updateReadinessCheck_resourceSetName = Lens.lens (\UpdateReadinessCheck' {resourceSetName} -> resourceSetName) (\s@UpdateReadinessCheck' {} a -> s {resourceSetName = a} :: UpdateReadinessCheck)

instance Core.AWSRequest UpdateReadinessCheck where
  type
    AWSResponse UpdateReadinessCheck =
      UpdateReadinessCheckResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReadinessCheckResponse'
            Prelude.<$> (x Data..?> "readinessCheckArn")
            Prelude.<*> (x Data..?> "readinessCheckName")
            Prelude.<*> (x Data..?> "resourceSet")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReadinessCheck where
  hashWithSalt _salt UpdateReadinessCheck' {..} =
    _salt
      `Prelude.hashWithSalt` readinessCheckName
      `Prelude.hashWithSalt` resourceSetName

instance Prelude.NFData UpdateReadinessCheck where
  rnf UpdateReadinessCheck' {..} =
    Prelude.rnf readinessCheckName `Prelude.seq`
      Prelude.rnf resourceSetName

instance Data.ToHeaders UpdateReadinessCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateReadinessCheck where
  toJSON UpdateReadinessCheck' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceSetName" Data..= resourceSetName)
          ]
      )

instance Data.ToPath UpdateReadinessCheck where
  toPath UpdateReadinessCheck' {..} =
    Prelude.mconcat
      ["/readinesschecks/", Data.toBS readinessCheckName]

instance Data.ToQuery UpdateReadinessCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReadinessCheckResponse' smart constructor.
data UpdateReadinessCheckResponse = UpdateReadinessCheckResponse'
  { -- | The Amazon Resource Name (ARN) associated with a readiness check.
    readinessCheckArn :: Prelude.Maybe Prelude.Text,
    -- | Name of a readiness check.
    readinessCheckName :: Prelude.Maybe Prelude.Text,
    -- | Name of the resource set to be checked.
    resourceSet :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReadinessCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessCheckArn', 'updateReadinessCheckResponse_readinessCheckArn' - The Amazon Resource Name (ARN) associated with a readiness check.
--
-- 'readinessCheckName', 'updateReadinessCheckResponse_readinessCheckName' - Name of a readiness check.
--
-- 'resourceSet', 'updateReadinessCheckResponse_resourceSet' - Name of the resource set to be checked.
--
-- 'tags', 'updateReadinessCheckResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'updateReadinessCheckResponse_httpStatus' - The response's http status code.
newUpdateReadinessCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateReadinessCheckResponse
newUpdateReadinessCheckResponse pHttpStatus_ =
  UpdateReadinessCheckResponse'
    { readinessCheckArn =
        Prelude.Nothing,
      readinessCheckName = Prelude.Nothing,
      resourceSet = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) associated with a readiness check.
updateReadinessCheckResponse_readinessCheckArn :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe Prelude.Text)
updateReadinessCheckResponse_readinessCheckArn = Lens.lens (\UpdateReadinessCheckResponse' {readinessCheckArn} -> readinessCheckArn) (\s@UpdateReadinessCheckResponse' {} a -> s {readinessCheckArn = a} :: UpdateReadinessCheckResponse)

-- | Name of a readiness check.
updateReadinessCheckResponse_readinessCheckName :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe Prelude.Text)
updateReadinessCheckResponse_readinessCheckName = Lens.lens (\UpdateReadinessCheckResponse' {readinessCheckName} -> readinessCheckName) (\s@UpdateReadinessCheckResponse' {} a -> s {readinessCheckName = a} :: UpdateReadinessCheckResponse)

-- | Name of the resource set to be checked.
updateReadinessCheckResponse_resourceSet :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe Prelude.Text)
updateReadinessCheckResponse_resourceSet = Lens.lens (\UpdateReadinessCheckResponse' {resourceSet} -> resourceSet) (\s@UpdateReadinessCheckResponse' {} a -> s {resourceSet = a} :: UpdateReadinessCheckResponse)

-- | Undocumented member.
updateReadinessCheckResponse_tags :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateReadinessCheckResponse_tags = Lens.lens (\UpdateReadinessCheckResponse' {tags} -> tags) (\s@UpdateReadinessCheckResponse' {} a -> s {tags = a} :: UpdateReadinessCheckResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateReadinessCheckResponse_httpStatus :: Lens.Lens' UpdateReadinessCheckResponse Prelude.Int
updateReadinessCheckResponse_httpStatus = Lens.lens (\UpdateReadinessCheckResponse' {httpStatus} -> httpStatus) (\s@UpdateReadinessCheckResponse' {} a -> s {httpStatus = a} :: UpdateReadinessCheckResponse)

instance Prelude.NFData UpdateReadinessCheckResponse where
  rnf UpdateReadinessCheckResponse' {..} =
    Prelude.rnf readinessCheckArn `Prelude.seq`
      Prelude.rnf readinessCheckName `Prelude.seq`
        Prelude.rnf resourceSet `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf httpStatus
