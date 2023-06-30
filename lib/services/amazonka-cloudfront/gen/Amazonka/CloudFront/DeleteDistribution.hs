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
-- Module      : Amazonka.CloudFront.DeleteDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a distribution.
module Amazonka.CloudFront.DeleteDistribution
  ( -- * Creating a Request
    DeleteDistribution (..),
    newDeleteDistribution,

    -- * Request Lenses
    deleteDistribution_ifMatch,
    deleteDistribution_id,

    -- * Destructuring the Response
    DeleteDistributionResponse (..),
    newDeleteDistributionResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This action deletes a web distribution. To delete a web distribution
-- using the CloudFront API, perform the following steps.
--
-- __To delete a web distribution using the CloudFront API:__
--
-- 1.  Disable the web distribution
--
-- 2.  Submit a @GET Distribution Config@ request to get the current
--     configuration and the @Etag@ header for the distribution.
--
-- 3.  Update the XML document that was returned in the response to your
--     @GET Distribution Config@ request to change the value of @Enabled@
--     to @false@.
--
-- 4.  Submit a @PUT Distribution Config@ request to update the
--     configuration for your distribution. In the request body, include
--     the XML document that you updated in Step 3. Set the value of the
--     HTTP @If-Match@ header to the value of the @ETag@ header that
--     CloudFront returned when you submitted the @GET Distribution Config@
--     request in Step 2.
--
-- 5.  Review the response to the @PUT Distribution Config@ request to
--     confirm that the distribution was successfully disabled.
--
-- 6.  Submit a @GET Distribution@ request to confirm that your changes
--     have propagated. When propagation is complete, the value of @Status@
--     is @Deployed@.
--
-- 7.  Submit a @DELETE Distribution@ request. Set the value of the HTTP
--     @If-Match@ header to the value of the @ETag@ header that CloudFront
--     returned when you submitted the @GET Distribution Config@ request in
--     Step 6.
--
-- 8.  Review the response to your @DELETE Distribution@ request to confirm
--     that the distribution was successfully deleted.
--
-- For information about deleting a distribution using the CloudFront
-- console, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html Deleting a Distribution>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newDeleteDistribution' smart constructor.
data DeleteDistribution = DeleteDistribution'
  { -- | The value of the @ETag@ header that you received when you disabled the
    -- distribution. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The distribution ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteDistribution_ifMatch' - The value of the @ETag@ header that you received when you disabled the
-- distribution. For example: @E2QWRUHAPOMQZL@.
--
-- 'id', 'deleteDistribution_id' - The distribution ID.
newDeleteDistribution ::
  -- | 'id'
  Prelude.Text ->
  DeleteDistribution
newDeleteDistribution pId_ =
  DeleteDistribution'
    { ifMatch = Prelude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when you disabled the
-- distribution. For example: @E2QWRUHAPOMQZL@.
deleteDistribution_ifMatch :: Lens.Lens' DeleteDistribution (Prelude.Maybe Prelude.Text)
deleteDistribution_ifMatch = Lens.lens (\DeleteDistribution' {ifMatch} -> ifMatch) (\s@DeleteDistribution' {} a -> s {ifMatch = a} :: DeleteDistribution)

-- | The distribution ID.
deleteDistribution_id :: Lens.Lens' DeleteDistribution Prelude.Text
deleteDistribution_id = Lens.lens (\DeleteDistribution' {id} -> id) (\s@DeleteDistribution' {} a -> s {id = a} :: DeleteDistribution)

instance Core.AWSRequest DeleteDistribution where
  type
    AWSResponse DeleteDistribution =
      DeleteDistributionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteDistributionResponse'

instance Prelude.Hashable DeleteDistribution where
  hashWithSalt _salt DeleteDistribution' {..} =
    _salt
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteDistribution where
  rnf DeleteDistribution' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteDistribution where
  toHeaders DeleteDistribution' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteDistribution where
  toPath DeleteDistribution' {..} =
    Prelude.mconcat
      ["/2020-05-31/distribution/", Data.toBS id]

instance Data.ToQuery DeleteDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDistributionResponse ::
  DeleteDistributionResponse
newDeleteDistributionResponse =
  DeleteDistributionResponse'

instance Prelude.NFData DeleteDistributionResponse where
  rnf _ = ()
