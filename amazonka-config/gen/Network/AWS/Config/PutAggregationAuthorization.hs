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
-- Module      : Network.AWS.Config.PutAggregationAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the aggregator account and region to collect data from the
-- source account and region.
module Network.AWS.Config.PutAggregationAuthorization
  ( -- * Creating a Request
    PutAggregationAuthorization (..),
    newPutAggregationAuthorization,

    -- * Request Lenses
    putAggregationAuthorization_tags,
    putAggregationAuthorization_authorizedAccountId,
    putAggregationAuthorization_authorizedAwsRegion,

    -- * Destructuring the Response
    PutAggregationAuthorizationResponse (..),
    newPutAggregationAuthorizationResponse,

    -- * Response Lenses
    putAggregationAuthorizationResponse_aggregationAuthorization,
    putAggregationAuthorizationResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAggregationAuthorization' smart constructor.
data PutAggregationAuthorization = PutAggregationAuthorization'
  { -- | An array of tag object.
    tags :: Prelude.Maybe [Tag],
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Prelude.Text,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAggregationAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putAggregationAuthorization_tags' - An array of tag object.
--
-- 'authorizedAccountId', 'putAggregationAuthorization_authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
--
-- 'authorizedAwsRegion', 'putAggregationAuthorization_authorizedAwsRegion' - The region authorized to collect aggregated data.
newPutAggregationAuthorization ::
  -- | 'authorizedAccountId'
  Prelude.Text ->
  -- | 'authorizedAwsRegion'
  Prelude.Text ->
  PutAggregationAuthorization
newPutAggregationAuthorization
  pAuthorizedAccountId_
  pAuthorizedAwsRegion_ =
    PutAggregationAuthorization'
      { tags =
          Prelude.Nothing,
        authorizedAccountId = pAuthorizedAccountId_,
        authorizedAwsRegion = pAuthorizedAwsRegion_
      }

-- | An array of tag object.
putAggregationAuthorization_tags :: Lens.Lens' PutAggregationAuthorization (Prelude.Maybe [Tag])
putAggregationAuthorization_tags = Lens.lens (\PutAggregationAuthorization' {tags} -> tags) (\s@PutAggregationAuthorization' {} a -> s {tags = a} :: PutAggregationAuthorization) Prelude.. Lens.mapping Lens._Coerce

-- | The 12-digit account ID of the account authorized to aggregate data.
putAggregationAuthorization_authorizedAccountId :: Lens.Lens' PutAggregationAuthorization Prelude.Text
putAggregationAuthorization_authorizedAccountId = Lens.lens (\PutAggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@PutAggregationAuthorization' {} a -> s {authorizedAccountId = a} :: PutAggregationAuthorization)

-- | The region authorized to collect aggregated data.
putAggregationAuthorization_authorizedAwsRegion :: Lens.Lens' PutAggregationAuthorization Prelude.Text
putAggregationAuthorization_authorizedAwsRegion = Lens.lens (\PutAggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@PutAggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: PutAggregationAuthorization)

instance Core.AWSRequest PutAggregationAuthorization where
  type
    AWSResponse PutAggregationAuthorization =
      PutAggregationAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAggregationAuthorizationResponse'
            Prelude.<$> (x Core..?> "AggregationAuthorization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAggregationAuthorization

instance Prelude.NFData PutAggregationAuthorization

instance Core.ToHeaders PutAggregationAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutAggregationAuthorization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAggregationAuthorization where
  toJSON PutAggregationAuthorization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("AuthorizedAccountId" Core..= authorizedAccountId),
            Prelude.Just
              ("AuthorizedAwsRegion" Core..= authorizedAwsRegion)
          ]
      )

instance Core.ToPath PutAggregationAuthorization where
  toPath = Prelude.const "/"

instance Core.ToQuery PutAggregationAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAggregationAuthorizationResponse' smart constructor.
data PutAggregationAuthorizationResponse = PutAggregationAuthorizationResponse'
  { -- | Returns an AggregationAuthorization object.
    aggregationAuthorization :: Prelude.Maybe AggregationAuthorization,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAggregationAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationAuthorization', 'putAggregationAuthorizationResponse_aggregationAuthorization' - Returns an AggregationAuthorization object.
--
-- 'httpStatus', 'putAggregationAuthorizationResponse_httpStatus' - The response's http status code.
newPutAggregationAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAggregationAuthorizationResponse
newPutAggregationAuthorizationResponse pHttpStatus_ =
  PutAggregationAuthorizationResponse'
    { aggregationAuthorization =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns an AggregationAuthorization object.
putAggregationAuthorizationResponse_aggregationAuthorization :: Lens.Lens' PutAggregationAuthorizationResponse (Prelude.Maybe AggregationAuthorization)
putAggregationAuthorizationResponse_aggregationAuthorization = Lens.lens (\PutAggregationAuthorizationResponse' {aggregationAuthorization} -> aggregationAuthorization) (\s@PutAggregationAuthorizationResponse' {} a -> s {aggregationAuthorization = a} :: PutAggregationAuthorizationResponse)

-- | The response's http status code.
putAggregationAuthorizationResponse_httpStatus :: Lens.Lens' PutAggregationAuthorizationResponse Prelude.Int
putAggregationAuthorizationResponse_httpStatus = Lens.lens (\PutAggregationAuthorizationResponse' {httpStatus} -> httpStatus) (\s@PutAggregationAuthorizationResponse' {} a -> s {httpStatus = a} :: PutAggregationAuthorizationResponse)

instance
  Prelude.NFData
    PutAggregationAuthorizationResponse
