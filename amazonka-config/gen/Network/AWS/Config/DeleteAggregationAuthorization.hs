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
-- Module      : Network.AWS.Config.DeleteAggregationAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the authorization granted to the specified configuration
-- aggregator account in a specified region.
module Network.AWS.Config.DeleteAggregationAuthorization
  ( -- * Creating a Request
    DeleteAggregationAuthorization (..),
    newDeleteAggregationAuthorization,

    -- * Request Lenses
    deleteAggregationAuthorization_authorizedAccountId,
    deleteAggregationAuthorization_authorizedAwsRegion,

    -- * Destructuring the Response
    DeleteAggregationAuthorizationResponse (..),
    newDeleteAggregationAuthorizationResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAggregationAuthorization' smart constructor.
data DeleteAggregationAuthorization = DeleteAggregationAuthorization'
  { -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Core.Text,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAggregationAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedAccountId', 'deleteAggregationAuthorization_authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
--
-- 'authorizedAwsRegion', 'deleteAggregationAuthorization_authorizedAwsRegion' - The region authorized to collect aggregated data.
newDeleteAggregationAuthorization ::
  -- | 'authorizedAccountId'
  Core.Text ->
  -- | 'authorizedAwsRegion'
  Core.Text ->
  DeleteAggregationAuthorization
newDeleteAggregationAuthorization
  pAuthorizedAccountId_
  pAuthorizedAwsRegion_ =
    DeleteAggregationAuthorization'
      { authorizedAccountId =
          pAuthorizedAccountId_,
        authorizedAwsRegion = pAuthorizedAwsRegion_
      }

-- | The 12-digit account ID of the account authorized to aggregate data.
deleteAggregationAuthorization_authorizedAccountId :: Lens.Lens' DeleteAggregationAuthorization Core.Text
deleteAggregationAuthorization_authorizedAccountId = Lens.lens (\DeleteAggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@DeleteAggregationAuthorization' {} a -> s {authorizedAccountId = a} :: DeleteAggregationAuthorization)

-- | The region authorized to collect aggregated data.
deleteAggregationAuthorization_authorizedAwsRegion :: Lens.Lens' DeleteAggregationAuthorization Core.Text
deleteAggregationAuthorization_authorizedAwsRegion = Lens.lens (\DeleteAggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@DeleteAggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: DeleteAggregationAuthorization)

instance
  Core.AWSRequest
    DeleteAggregationAuthorization
  where
  type
    AWSResponse DeleteAggregationAuthorization =
      DeleteAggregationAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteAggregationAuthorizationResponse'

instance Core.Hashable DeleteAggregationAuthorization

instance Core.NFData DeleteAggregationAuthorization

instance
  Core.ToHeaders
    DeleteAggregationAuthorization
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteAggregationAuthorization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAggregationAuthorization where
  toJSON DeleteAggregationAuthorization' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AuthorizedAccountId" Core..= authorizedAccountId),
            Core.Just
              ("AuthorizedAwsRegion" Core..= authorizedAwsRegion)
          ]
      )

instance Core.ToPath DeleteAggregationAuthorization where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAggregationAuthorization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAggregationAuthorizationResponse' smart constructor.
data DeleteAggregationAuthorizationResponse = DeleteAggregationAuthorizationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAggregationAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAggregationAuthorizationResponse ::
  DeleteAggregationAuthorizationResponse
newDeleteAggregationAuthorizationResponse =
  DeleteAggregationAuthorizationResponse'

instance
  Core.NFData
    DeleteAggregationAuthorizationResponse
