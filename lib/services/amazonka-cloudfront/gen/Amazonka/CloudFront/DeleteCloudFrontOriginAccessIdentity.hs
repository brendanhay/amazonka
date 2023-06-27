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
-- Module      : Amazonka.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an origin access identity.
module Amazonka.CloudFront.DeleteCloudFrontOriginAccessIdentity
  ( -- * Creating a Request
    DeleteCloudFrontOriginAccessIdentity (..),
    newDeleteCloudFrontOriginAccessIdentity,

    -- * Request Lenses
    deleteCloudFrontOriginAccessIdentity_ifMatch,
    deleteCloudFrontOriginAccessIdentity_id,

    -- * Destructuring the Response
    DeleteCloudFrontOriginAccessIdentityResponse (..),
    newDeleteCloudFrontOriginAccessIdentityResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes a origin access identity.
--
-- /See:/ 'newDeleteCloudFrontOriginAccessIdentity' smart constructor.
data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity'
  { -- | The value of the @ETag@ header you received from a previous @GET@ or
    -- @PUT@ request. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The origin access identity\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCloudFrontOriginAccessIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteCloudFrontOriginAccessIdentity_ifMatch' - The value of the @ETag@ header you received from a previous @GET@ or
-- @PUT@ request. For example: @E2QWRUHAPOMQZL@.
--
-- 'id', 'deleteCloudFrontOriginAccessIdentity_id' - The origin access identity\'s ID.
newDeleteCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Prelude.Text ->
  DeleteCloudFrontOriginAccessIdentity
newDeleteCloudFrontOriginAccessIdentity pId_ =
  DeleteCloudFrontOriginAccessIdentity'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header you received from a previous @GET@ or
-- @PUT@ request. For example: @E2QWRUHAPOMQZL@.
deleteCloudFrontOriginAccessIdentity_ifMatch :: Lens.Lens' DeleteCloudFrontOriginAccessIdentity (Prelude.Maybe Prelude.Text)
deleteCloudFrontOriginAccessIdentity_ifMatch = Lens.lens (\DeleteCloudFrontOriginAccessIdentity' {ifMatch} -> ifMatch) (\s@DeleteCloudFrontOriginAccessIdentity' {} a -> s {ifMatch = a} :: DeleteCloudFrontOriginAccessIdentity)

-- | The origin access identity\'s ID.
deleteCloudFrontOriginAccessIdentity_id :: Lens.Lens' DeleteCloudFrontOriginAccessIdentity Prelude.Text
deleteCloudFrontOriginAccessIdentity_id = Lens.lens (\DeleteCloudFrontOriginAccessIdentity' {id} -> id) (\s@DeleteCloudFrontOriginAccessIdentity' {} a -> s {id = a} :: DeleteCloudFrontOriginAccessIdentity)

instance
  Core.AWSRequest
    DeleteCloudFrontOriginAccessIdentity
  where
  type
    AWSResponse DeleteCloudFrontOriginAccessIdentity =
      DeleteCloudFrontOriginAccessIdentityResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCloudFrontOriginAccessIdentityResponse'

instance
  Prelude.Hashable
    DeleteCloudFrontOriginAccessIdentity
  where
  hashWithSalt
    _salt
    DeleteCloudFrontOriginAccessIdentity' {..} =
      _salt
        `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteCloudFrontOriginAccessIdentity
  where
  rnf DeleteCloudFrontOriginAccessIdentity' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DeleteCloudFrontOriginAccessIdentity
  where
  toHeaders DeleteCloudFrontOriginAccessIdentity' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance
  Data.ToPath
    DeleteCloudFrontOriginAccessIdentity
  where
  toPath DeleteCloudFrontOriginAccessIdentity' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Data.toBS id
      ]

instance
  Data.ToQuery
    DeleteCloudFrontOriginAccessIdentity
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCloudFrontOriginAccessIdentityResponse' smart constructor.
data DeleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCloudFrontOriginAccessIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCloudFrontOriginAccessIdentityResponse ::
  DeleteCloudFrontOriginAccessIdentityResponse
newDeleteCloudFrontOriginAccessIdentityResponse =
  DeleteCloudFrontOriginAccessIdentityResponse'

instance
  Prelude.NFData
    DeleteCloudFrontOriginAccessIdentityResponse
  where
  rnf _ = ()
