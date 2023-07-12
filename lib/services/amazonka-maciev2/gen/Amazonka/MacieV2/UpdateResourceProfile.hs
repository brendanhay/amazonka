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
-- Module      : Amazonka.MacieV2.UpdateResourceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the sensitivity score for an S3 bucket.
module Amazonka.MacieV2.UpdateResourceProfile
  ( -- * Creating a Request
    UpdateResourceProfile (..),
    newUpdateResourceProfile,

    -- * Request Lenses
    updateResourceProfile_sensitivityScoreOverride,
    updateResourceProfile_resourceArn,

    -- * Destructuring the Response
    UpdateResourceProfileResponse (..),
    newUpdateResourceProfileResponse,

    -- * Response Lenses
    updateResourceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourceProfile' smart constructor.
data UpdateResourceProfile = UpdateResourceProfile'
  { -- | The new sensitivity score for the bucket. Valid values are: 100, assign
    -- the maximum score and apply the /Sensitive/ label to the bucket; and,
    -- null (empty), assign a score that Amazon Macie calculates automatically
    -- after you submit the request.
    sensitivityScoreOverride :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
    -- to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sensitivityScoreOverride', 'updateResourceProfile_sensitivityScoreOverride' - The new sensitivity score for the bucket. Valid values are: 100, assign
-- the maximum score and apply the /Sensitive/ label to the bucket; and,
-- null (empty), assign a score that Amazon Macie calculates automatically
-- after you submit the request.
--
-- 'resourceArn', 'updateResourceProfile_resourceArn' - The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
newUpdateResourceProfile ::
  -- | 'resourceArn'
  Prelude.Text ->
  UpdateResourceProfile
newUpdateResourceProfile pResourceArn_ =
  UpdateResourceProfile'
    { sensitivityScoreOverride =
        Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The new sensitivity score for the bucket. Valid values are: 100, assign
-- the maximum score and apply the /Sensitive/ label to the bucket; and,
-- null (empty), assign a score that Amazon Macie calculates automatically
-- after you submit the request.
updateResourceProfile_sensitivityScoreOverride :: Lens.Lens' UpdateResourceProfile (Prelude.Maybe Prelude.Int)
updateResourceProfile_sensitivityScoreOverride = Lens.lens (\UpdateResourceProfile' {sensitivityScoreOverride} -> sensitivityScoreOverride) (\s@UpdateResourceProfile' {} a -> s {sensitivityScoreOverride = a} :: UpdateResourceProfile)

-- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
updateResourceProfile_resourceArn :: Lens.Lens' UpdateResourceProfile Prelude.Text
updateResourceProfile_resourceArn = Lens.lens (\UpdateResourceProfile' {resourceArn} -> resourceArn) (\s@UpdateResourceProfile' {} a -> s {resourceArn = a} :: UpdateResourceProfile)

instance Core.AWSRequest UpdateResourceProfile where
  type
    AWSResponse UpdateResourceProfile =
      UpdateResourceProfileResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceProfile where
  hashWithSalt _salt UpdateResourceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` sensitivityScoreOverride
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData UpdateResourceProfile where
  rnf UpdateResourceProfile' {..} =
    Prelude.rnf sensitivityScoreOverride
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders UpdateResourceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResourceProfile where
  toJSON UpdateResourceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sensitivityScoreOverride" Data..=)
              Prelude.<$> sensitivityScoreOverride
          ]
      )

instance Data.ToPath UpdateResourceProfile where
  toPath = Prelude.const "/resource-profiles"

instance Data.ToQuery UpdateResourceProfile where
  toQuery UpdateResourceProfile' {..} =
    Prelude.mconcat ["resourceArn" Data.=: resourceArn]

-- | /See:/ 'newUpdateResourceProfileResponse' smart constructor.
data UpdateResourceProfileResponse = UpdateResourceProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceProfileResponse_httpStatus' - The response's http status code.
newUpdateResourceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceProfileResponse
newUpdateResourceProfileResponse pHttpStatus_ =
  UpdateResourceProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResourceProfileResponse_httpStatus :: Lens.Lens' UpdateResourceProfileResponse Prelude.Int
updateResourceProfileResponse_httpStatus = Lens.lens (\UpdateResourceProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceProfileResponse' {} a -> s {httpStatus = a} :: UpdateResourceProfileResponse)

instance Prelude.NFData UpdateResourceProfileResponse where
  rnf UpdateResourceProfileResponse' {..} =
    Prelude.rnf httpStatus
