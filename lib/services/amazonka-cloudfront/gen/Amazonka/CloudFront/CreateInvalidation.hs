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
-- Module      : Amazonka.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new invalidation.
module Amazonka.CloudFront.CreateInvalidation
  ( -- * Creating a Request
    CreateInvalidation (..),
    newCreateInvalidation,

    -- * Request Lenses
    createInvalidation_distributionId,
    createInvalidation_invalidationBatch,

    -- * Destructuring the Response
    CreateInvalidationResponse (..),
    newCreateInvalidationResponse,

    -- * Response Lenses
    createInvalidationResponse_location,
    createInvalidationResponse_invalidation,
    createInvalidationResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to create an invalidation.
--
-- /See:/ 'newCreateInvalidation' smart constructor.
data CreateInvalidation = CreateInvalidation'
  { -- | The distribution\'s id.
    distributionId :: Prelude.Text,
    -- | The batch information for the invalidation.
    invalidationBatch :: InvalidationBatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInvalidation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionId', 'createInvalidation_distributionId' - The distribution\'s id.
--
-- 'invalidationBatch', 'createInvalidation_invalidationBatch' - The batch information for the invalidation.
newCreateInvalidation ::
  -- | 'distributionId'
  Prelude.Text ->
  -- | 'invalidationBatch'
  InvalidationBatch ->
  CreateInvalidation
newCreateInvalidation
  pDistributionId_
  pInvalidationBatch_ =
    CreateInvalidation'
      { distributionId =
          pDistributionId_,
        invalidationBatch = pInvalidationBatch_
      }

-- | The distribution\'s id.
createInvalidation_distributionId :: Lens.Lens' CreateInvalidation Prelude.Text
createInvalidation_distributionId = Lens.lens (\CreateInvalidation' {distributionId} -> distributionId) (\s@CreateInvalidation' {} a -> s {distributionId = a} :: CreateInvalidation)

-- | The batch information for the invalidation.
createInvalidation_invalidationBatch :: Lens.Lens' CreateInvalidation InvalidationBatch
createInvalidation_invalidationBatch = Lens.lens (\CreateInvalidation' {invalidationBatch} -> invalidationBatch) (\s@CreateInvalidation' {} a -> s {invalidationBatch = a} :: CreateInvalidation)

instance Core.AWSRequest CreateInvalidation where
  type
    AWSResponse CreateInvalidation =
      CreateInvalidationResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInvalidationResponse'
            Prelude.<$> (h Core..#? "Location")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInvalidation where
  hashWithSalt _salt CreateInvalidation' {..} =
    _salt `Prelude.hashWithSalt` distributionId
      `Prelude.hashWithSalt` invalidationBatch

instance Prelude.NFData CreateInvalidation where
  rnf CreateInvalidation' {..} =
    Prelude.rnf distributionId
      `Prelude.seq` Prelude.rnf invalidationBatch

instance Core.ToElement CreateInvalidation where
  toElement CreateInvalidation' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}InvalidationBatch"
      invalidationBatch

instance Core.ToHeaders CreateInvalidation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateInvalidation where
  toPath CreateInvalidation' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Core.toBS distributionId,
        "/invalidation"
      ]

instance Core.ToQuery CreateInvalidation where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateInvalidationResponse' smart constructor.
data CreateInvalidationResponse = CreateInvalidationResponse'
  { -- | The fully qualified URI of the distribution and invalidation batch
    -- request, including the @Invalidation ID@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The invalidation\'s information.
    invalidation :: Prelude.Maybe Invalidation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInvalidationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createInvalidationResponse_location' - The fully qualified URI of the distribution and invalidation batch
-- request, including the @Invalidation ID@.
--
-- 'invalidation', 'createInvalidationResponse_invalidation' - The invalidation\'s information.
--
-- 'httpStatus', 'createInvalidationResponse_httpStatus' - The response's http status code.
newCreateInvalidationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInvalidationResponse
newCreateInvalidationResponse pHttpStatus_ =
  CreateInvalidationResponse'
    { location =
        Prelude.Nothing,
      invalidation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The fully qualified URI of the distribution and invalidation batch
-- request, including the @Invalidation ID@.
createInvalidationResponse_location :: Lens.Lens' CreateInvalidationResponse (Prelude.Maybe Prelude.Text)
createInvalidationResponse_location = Lens.lens (\CreateInvalidationResponse' {location} -> location) (\s@CreateInvalidationResponse' {} a -> s {location = a} :: CreateInvalidationResponse)

-- | The invalidation\'s information.
createInvalidationResponse_invalidation :: Lens.Lens' CreateInvalidationResponse (Prelude.Maybe Invalidation)
createInvalidationResponse_invalidation = Lens.lens (\CreateInvalidationResponse' {invalidation} -> invalidation) (\s@CreateInvalidationResponse' {} a -> s {invalidation = a} :: CreateInvalidationResponse)

-- | The response's http status code.
createInvalidationResponse_httpStatus :: Lens.Lens' CreateInvalidationResponse Prelude.Int
createInvalidationResponse_httpStatus = Lens.lens (\CreateInvalidationResponse' {httpStatus} -> httpStatus) (\s@CreateInvalidationResponse' {} a -> s {httpStatus = a} :: CreateInvalidationResponse)

instance Prelude.NFData CreateInvalidationResponse where
  rnf CreateInvalidationResponse' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf invalidation
      `Prelude.seq` Prelude.rnf httpStatus
