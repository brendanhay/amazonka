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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an exisiting Readiness Check.
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
    updateReadinessCheckResponse_readinessCheckName,
    updateReadinessCheckResponse_resourceSet,
    updateReadinessCheckResponse_readinessCheckArn,
    updateReadinessCheckResponse_tags,
    updateReadinessCheckResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | The new Readiness Check values
--
-- /See:/ 'newUpdateReadinessCheck' smart constructor.
data UpdateReadinessCheck = UpdateReadinessCheck'
  { -- | The ReadinessCheck to update
    readinessCheckName :: Prelude.Text,
    -- | The name of the ResourceSet to check
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
-- 'readinessCheckName', 'updateReadinessCheck_readinessCheckName' - The ReadinessCheck to update
--
-- 'resourceSetName', 'updateReadinessCheck_resourceSetName' - The name of the ResourceSet to check
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

-- | The ReadinessCheck to update
updateReadinessCheck_readinessCheckName :: Lens.Lens' UpdateReadinessCheck Prelude.Text
updateReadinessCheck_readinessCheckName = Lens.lens (\UpdateReadinessCheck' {readinessCheckName} -> readinessCheckName) (\s@UpdateReadinessCheck' {} a -> s {readinessCheckName = a} :: UpdateReadinessCheck)

-- | The name of the ResourceSet to check
updateReadinessCheck_resourceSetName :: Lens.Lens' UpdateReadinessCheck Prelude.Text
updateReadinessCheck_resourceSetName = Lens.lens (\UpdateReadinessCheck' {resourceSetName} -> resourceSetName) (\s@UpdateReadinessCheck' {} a -> s {resourceSetName = a} :: UpdateReadinessCheck)

instance Core.AWSRequest UpdateReadinessCheck where
  type
    AWSResponse UpdateReadinessCheck =
      UpdateReadinessCheckResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReadinessCheckResponse'
            Prelude.<$> (x Core..?> "readinessCheckName")
            Prelude.<*> (x Core..?> "resourceSet")
            Prelude.<*> (x Core..?> "readinessCheckArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReadinessCheck where
  hashWithSalt _salt UpdateReadinessCheck' {..} =
    _salt `Prelude.hashWithSalt` readinessCheckName
      `Prelude.hashWithSalt` resourceSetName

instance Prelude.NFData UpdateReadinessCheck where
  rnf UpdateReadinessCheck' {..} =
    Prelude.rnf readinessCheckName
      `Prelude.seq` Prelude.rnf resourceSetName

instance Core.ToHeaders UpdateReadinessCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateReadinessCheck where
  toJSON UpdateReadinessCheck' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceSetName" Core..= resourceSetName)
          ]
      )

instance Core.ToPath UpdateReadinessCheck where
  toPath UpdateReadinessCheck' {..} =
    Prelude.mconcat
      ["/readinesschecks/", Core.toBS readinessCheckName]

instance Core.ToQuery UpdateReadinessCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReadinessCheckResponse' smart constructor.
data UpdateReadinessCheckResponse = UpdateReadinessCheckResponse'
  { -- | Name for a ReadinessCheck
    readinessCheckName :: Prelude.Maybe Prelude.Text,
    -- | Name of the ResourceSet to be checked
    resourceSet :: Prelude.Maybe Prelude.Text,
    -- | Arn associated with ReadinessCheck
    readinessCheckArn :: Prelude.Maybe Prelude.Text,
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
-- 'readinessCheckName', 'updateReadinessCheckResponse_readinessCheckName' - Name for a ReadinessCheck
--
-- 'resourceSet', 'updateReadinessCheckResponse_resourceSet' - Name of the ResourceSet to be checked
--
-- 'readinessCheckArn', 'updateReadinessCheckResponse_readinessCheckArn' - Arn associated with ReadinessCheck
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
    { readinessCheckName =
        Prelude.Nothing,
      resourceSet = Prelude.Nothing,
      readinessCheckArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Name for a ReadinessCheck
updateReadinessCheckResponse_readinessCheckName :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe Prelude.Text)
updateReadinessCheckResponse_readinessCheckName = Lens.lens (\UpdateReadinessCheckResponse' {readinessCheckName} -> readinessCheckName) (\s@UpdateReadinessCheckResponse' {} a -> s {readinessCheckName = a} :: UpdateReadinessCheckResponse)

-- | Name of the ResourceSet to be checked
updateReadinessCheckResponse_resourceSet :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe Prelude.Text)
updateReadinessCheckResponse_resourceSet = Lens.lens (\UpdateReadinessCheckResponse' {resourceSet} -> resourceSet) (\s@UpdateReadinessCheckResponse' {} a -> s {resourceSet = a} :: UpdateReadinessCheckResponse)

-- | Arn associated with ReadinessCheck
updateReadinessCheckResponse_readinessCheckArn :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe Prelude.Text)
updateReadinessCheckResponse_readinessCheckArn = Lens.lens (\UpdateReadinessCheckResponse' {readinessCheckArn} -> readinessCheckArn) (\s@UpdateReadinessCheckResponse' {} a -> s {readinessCheckArn = a} :: UpdateReadinessCheckResponse)

-- | Undocumented member.
updateReadinessCheckResponse_tags :: Lens.Lens' UpdateReadinessCheckResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateReadinessCheckResponse_tags = Lens.lens (\UpdateReadinessCheckResponse' {tags} -> tags) (\s@UpdateReadinessCheckResponse' {} a -> s {tags = a} :: UpdateReadinessCheckResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateReadinessCheckResponse_httpStatus :: Lens.Lens' UpdateReadinessCheckResponse Prelude.Int
updateReadinessCheckResponse_httpStatus = Lens.lens (\UpdateReadinessCheckResponse' {httpStatus} -> httpStatus) (\s@UpdateReadinessCheckResponse' {} a -> s {httpStatus = a} :: UpdateReadinessCheckResponse)

instance Prelude.NFData UpdateReadinessCheckResponse where
  rnf UpdateReadinessCheckResponse' {..} =
    Prelude.rnf readinessCheckName
      `Prelude.seq` Prelude.rnf resourceSet
      `Prelude.seq` Prelude.rnf readinessCheckArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
