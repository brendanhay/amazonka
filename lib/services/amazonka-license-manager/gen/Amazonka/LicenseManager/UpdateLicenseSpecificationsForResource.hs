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
-- Module      : Amazonka.LicenseManager.UpdateLicenseSpecificationsForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes the specified license configurations for the specified
-- Amazon Web Services resource.
--
-- You can update the license specifications of AMIs, instances, and hosts.
-- You cannot update the license specifications for launch templates and
-- CloudFormation templates, as they send license configurations to the
-- operation that creates the resource.
module Amazonka.LicenseManager.UpdateLicenseSpecificationsForResource
  ( -- * Creating a Request
    UpdateLicenseSpecificationsForResource (..),
    newUpdateLicenseSpecificationsForResource,

    -- * Request Lenses
    updateLicenseSpecificationsForResource_addLicenseSpecifications,
    updateLicenseSpecificationsForResource_removeLicenseSpecifications,
    updateLicenseSpecificationsForResource_resourceArn,

    -- * Destructuring the Response
    UpdateLicenseSpecificationsForResourceResponse (..),
    newUpdateLicenseSpecificationsForResourceResponse,

    -- * Response Lenses
    updateLicenseSpecificationsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLicenseSpecificationsForResource' smart constructor.
data UpdateLicenseSpecificationsForResource = UpdateLicenseSpecificationsForResource'
  { -- | ARNs of the license configurations to add.
    addLicenseSpecifications :: Prelude.Maybe [LicenseSpecification],
    -- | ARNs of the license configurations to remove.
    removeLicenseSpecifications :: Prelude.Maybe [LicenseSpecification],
    -- | Amazon Resource Name (ARN) of the Amazon Web Services resource.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLicenseSpecificationsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addLicenseSpecifications', 'updateLicenseSpecificationsForResource_addLicenseSpecifications' - ARNs of the license configurations to add.
--
-- 'removeLicenseSpecifications', 'updateLicenseSpecificationsForResource_removeLicenseSpecifications' - ARNs of the license configurations to remove.
--
-- 'resourceArn', 'updateLicenseSpecificationsForResource_resourceArn' - Amazon Resource Name (ARN) of the Amazon Web Services resource.
newUpdateLicenseSpecificationsForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  UpdateLicenseSpecificationsForResource
newUpdateLicenseSpecificationsForResource
  pResourceArn_ =
    UpdateLicenseSpecificationsForResource'
      { addLicenseSpecifications =
          Prelude.Nothing,
        removeLicenseSpecifications =
          Prelude.Nothing,
        resourceArn = pResourceArn_
      }

-- | ARNs of the license configurations to add.
updateLicenseSpecificationsForResource_addLicenseSpecifications :: Lens.Lens' UpdateLicenseSpecificationsForResource (Prelude.Maybe [LicenseSpecification])
updateLicenseSpecificationsForResource_addLicenseSpecifications = Lens.lens (\UpdateLicenseSpecificationsForResource' {addLicenseSpecifications} -> addLicenseSpecifications) (\s@UpdateLicenseSpecificationsForResource' {} a -> s {addLicenseSpecifications = a} :: UpdateLicenseSpecificationsForResource) Prelude.. Lens.mapping Lens.coerced

-- | ARNs of the license configurations to remove.
updateLicenseSpecificationsForResource_removeLicenseSpecifications :: Lens.Lens' UpdateLicenseSpecificationsForResource (Prelude.Maybe [LicenseSpecification])
updateLicenseSpecificationsForResource_removeLicenseSpecifications = Lens.lens (\UpdateLicenseSpecificationsForResource' {removeLicenseSpecifications} -> removeLicenseSpecifications) (\s@UpdateLicenseSpecificationsForResource' {} a -> s {removeLicenseSpecifications = a} :: UpdateLicenseSpecificationsForResource) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Name (ARN) of the Amazon Web Services resource.
updateLicenseSpecificationsForResource_resourceArn :: Lens.Lens' UpdateLicenseSpecificationsForResource Prelude.Text
updateLicenseSpecificationsForResource_resourceArn = Lens.lens (\UpdateLicenseSpecificationsForResource' {resourceArn} -> resourceArn) (\s@UpdateLicenseSpecificationsForResource' {} a -> s {resourceArn = a} :: UpdateLicenseSpecificationsForResource)

instance
  Core.AWSRequest
    UpdateLicenseSpecificationsForResource
  where
  type
    AWSResponse
      UpdateLicenseSpecificationsForResource =
      UpdateLicenseSpecificationsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLicenseSpecificationsForResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateLicenseSpecificationsForResource
  where
  hashWithSalt
    _salt
    UpdateLicenseSpecificationsForResource' {..} =
      _salt
        `Prelude.hashWithSalt` addLicenseSpecifications
        `Prelude.hashWithSalt` removeLicenseSpecifications
        `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    UpdateLicenseSpecificationsForResource
  where
  rnf UpdateLicenseSpecificationsForResource' {..} =
    Prelude.rnf addLicenseSpecifications
      `Prelude.seq` Prelude.rnf removeLicenseSpecifications
      `Prelude.seq` Prelude.rnf resourceArn

instance
  Data.ToHeaders
    UpdateLicenseSpecificationsForResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.UpdateLicenseSpecificationsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateLicenseSpecificationsForResource
  where
  toJSON UpdateLicenseSpecificationsForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddLicenseSpecifications" Data..=)
              Prelude.<$> addLicenseSpecifications,
            ("RemoveLicenseSpecifications" Data..=)
              Prelude.<$> removeLicenseSpecifications,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance
  Data.ToPath
    UpdateLicenseSpecificationsForResource
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateLicenseSpecificationsForResource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLicenseSpecificationsForResourceResponse' smart constructor.
data UpdateLicenseSpecificationsForResourceResponse = UpdateLicenseSpecificationsForResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLicenseSpecificationsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLicenseSpecificationsForResourceResponse_httpStatus' - The response's http status code.
newUpdateLicenseSpecificationsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLicenseSpecificationsForResourceResponse
newUpdateLicenseSpecificationsForResourceResponse
  pHttpStatus_ =
    UpdateLicenseSpecificationsForResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateLicenseSpecificationsForResourceResponse_httpStatus :: Lens.Lens' UpdateLicenseSpecificationsForResourceResponse Prelude.Int
updateLicenseSpecificationsForResourceResponse_httpStatus = Lens.lens (\UpdateLicenseSpecificationsForResourceResponse' {httpStatus} -> httpStatus) (\s@UpdateLicenseSpecificationsForResourceResponse' {} a -> s {httpStatus = a} :: UpdateLicenseSpecificationsForResourceResponse)

instance
  Prelude.NFData
    UpdateLicenseSpecificationsForResourceResponse
  where
  rnf
    UpdateLicenseSpecificationsForResourceResponse' {..} =
      Prelude.rnf httpStatus
