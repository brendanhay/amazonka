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
-- Module      : Network.AWS.CloudFront.GetInvalidation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an invalidation.
module Network.AWS.CloudFront.GetInvalidation
  ( -- * Creating a Request
    GetInvalidation (..),
    newGetInvalidation,

    -- * Request Lenses
    getInvalidation_distributionId,
    getInvalidation_id,

    -- * Destructuring the Response
    GetInvalidationResponse (..),
    newGetInvalidationResponse,

    -- * Response Lenses
    getInvalidationResponse_invalidation,
    getInvalidationResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get an invalidation\'s information.
--
-- /See:/ 'newGetInvalidation' smart constructor.
data GetInvalidation = GetInvalidation'
  { -- | The distribution\'s ID.
    distributionId :: Prelude.Text,
    -- | The identifier for the invalidation request, for example,
    -- @IDFDVBD632BHDS5@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInvalidation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionId', 'getInvalidation_distributionId' - The distribution\'s ID.
--
-- 'id', 'getInvalidation_id' - The identifier for the invalidation request, for example,
-- @IDFDVBD632BHDS5@.
newGetInvalidation ::
  -- | 'distributionId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetInvalidation
newGetInvalidation pDistributionId_ pId_ =
  GetInvalidation'
    { distributionId = pDistributionId_,
      id = pId_
    }

-- | The distribution\'s ID.
getInvalidation_distributionId :: Lens.Lens' GetInvalidation Prelude.Text
getInvalidation_distributionId = Lens.lens (\GetInvalidation' {distributionId} -> distributionId) (\s@GetInvalidation' {} a -> s {distributionId = a} :: GetInvalidation)

-- | The identifier for the invalidation request, for example,
-- @IDFDVBD632BHDS5@.
getInvalidation_id :: Lens.Lens' GetInvalidation Prelude.Text
getInvalidation_id = Lens.lens (\GetInvalidation' {id} -> id) (\s@GetInvalidation' {} a -> s {id = a} :: GetInvalidation)

instance Core.AWSRequest GetInvalidation where
  type
    AWSResponse GetInvalidation =
      GetInvalidationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetInvalidationResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInvalidation

instance Prelude.NFData GetInvalidation

instance Core.ToHeaders GetInvalidation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetInvalidation where
  toPath GetInvalidation' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Core.toBS distributionId,
        "/invalidation/",
        Core.toBS id
      ]

instance Core.ToQuery GetInvalidation where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetInvalidationResponse' smart constructor.
data GetInvalidationResponse = GetInvalidationResponse'
  { -- | The invalidation\'s information. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type>.
    invalidation :: Prelude.Maybe Invalidation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInvalidationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidation', 'getInvalidationResponse_invalidation' - The invalidation\'s information. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type>.
--
-- 'httpStatus', 'getInvalidationResponse_httpStatus' - The response's http status code.
newGetInvalidationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInvalidationResponse
newGetInvalidationResponse pHttpStatus_ =
  GetInvalidationResponse'
    { invalidation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The invalidation\'s information. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/InvalidationDatatype.html Invalidation Complex Type>.
getInvalidationResponse_invalidation :: Lens.Lens' GetInvalidationResponse (Prelude.Maybe Invalidation)
getInvalidationResponse_invalidation = Lens.lens (\GetInvalidationResponse' {invalidation} -> invalidation) (\s@GetInvalidationResponse' {} a -> s {invalidation = a} :: GetInvalidationResponse)

-- | The response's http status code.
getInvalidationResponse_httpStatus :: Lens.Lens' GetInvalidationResponse Prelude.Int
getInvalidationResponse_httpStatus = Lens.lens (\GetInvalidationResponse' {httpStatus} -> httpStatus) (\s@GetInvalidationResponse' {} a -> s {httpStatus = a} :: GetInvalidationResponse)

instance Prelude.NFData GetInvalidationResponse
