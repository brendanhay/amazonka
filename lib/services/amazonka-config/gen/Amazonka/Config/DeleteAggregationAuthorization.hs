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
-- Module      : Amazonka.Config.DeleteAggregationAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the authorization granted to the specified configuration
-- aggregator account in a specified region.
module Amazonka.Config.DeleteAggregationAuthorization
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAggregationAuthorization' smart constructor.
data DeleteAggregationAuthorization = DeleteAggregationAuthorization'
  { -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Prelude.Text,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'authorizedAwsRegion'
  Prelude.Text ->
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
deleteAggregationAuthorization_authorizedAccountId :: Lens.Lens' DeleteAggregationAuthorization Prelude.Text
deleteAggregationAuthorization_authorizedAccountId = Lens.lens (\DeleteAggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@DeleteAggregationAuthorization' {} a -> s {authorizedAccountId = a} :: DeleteAggregationAuthorization)

-- | The region authorized to collect aggregated data.
deleteAggregationAuthorization_authorizedAwsRegion :: Lens.Lens' DeleteAggregationAuthorization Prelude.Text
deleteAggregationAuthorization_authorizedAwsRegion = Lens.lens (\DeleteAggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@DeleteAggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: DeleteAggregationAuthorization)

instance
  Core.AWSRequest
    DeleteAggregationAuthorization
  where
  type
    AWSResponse DeleteAggregationAuthorization =
      DeleteAggregationAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAggregationAuthorizationResponse'

instance
  Prelude.Hashable
    DeleteAggregationAuthorization
  where
  hashWithSalt
    _salt
    DeleteAggregationAuthorization' {..} =
      _salt
        `Prelude.hashWithSalt` authorizedAccountId
        `Prelude.hashWithSalt` authorizedAwsRegion

instance
  Prelude.NFData
    DeleteAggregationAuthorization
  where
  rnf DeleteAggregationAuthorization' {..} =
    Prelude.rnf authorizedAccountId
      `Prelude.seq` Prelude.rnf authorizedAwsRegion

instance
  Data.ToHeaders
    DeleteAggregationAuthorization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteAggregationAuthorization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAggregationAuthorization where
  toJSON DeleteAggregationAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AuthorizedAccountId" Data..= authorizedAccountId),
            Prelude.Just
              ("AuthorizedAwsRegion" Data..= authorizedAwsRegion)
          ]
      )

instance Data.ToPath DeleteAggregationAuthorization where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAggregationAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAggregationAuthorizationResponse' smart constructor.
data DeleteAggregationAuthorizationResponse = DeleteAggregationAuthorizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAggregationAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAggregationAuthorizationResponse ::
  DeleteAggregationAuthorizationResponse
newDeleteAggregationAuthorizationResponse =
  DeleteAggregationAuthorizationResponse'

instance
  Prelude.NFData
    DeleteAggregationAuthorizationResponse
  where
  rnf _ = ()
