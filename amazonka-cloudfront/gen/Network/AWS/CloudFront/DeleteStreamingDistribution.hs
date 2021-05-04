{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a streaming distribution. To delete an RTMP distribution using
-- the CloudFront API, perform the following steps.
--
-- __To delete an RTMP distribution using the CloudFront API__:
--
-- 1.  Disable the RTMP distribution.
--
-- 2.  Submit a @GET Streaming Distribution Config@ request to get the
--     current configuration and the @Etag@ header for the distribution.
--
-- 3.  Update the XML document that was returned in the response to your
--     @GET Streaming Distribution Config@ request to change the value of
--     @Enabled@ to @false@.
--
-- 4.  Submit a @PUT Streaming Distribution Config@ request to update the
--     configuration for your distribution. In the request body, include
--     the XML document that you updated in Step 3. Then set the value of
--     the HTTP @If-Match@ header to the value of the @ETag@ header that
--     CloudFront returned when you submitted the
--     @GET Streaming Distribution Config@ request in Step 2.
--
-- 5.  Review the response to the @PUT Streaming Distribution Config@
--     request to confirm that the distribution was successfully disabled.
--
-- 6.  Submit a @GET Streaming Distribution Config@ request to confirm that
--     your changes have propagated. When propagation is complete, the
--     value of @Status@ is @Deployed@.
--
-- 7.  Submit a @DELETE Streaming Distribution@ request. Set the value of
--     the HTTP @If-Match@ header to the value of the @ETag@ header that
--     CloudFront returned when you submitted the
--     @GET Streaming Distribution Config@ request in Step 2.
--
-- 8.  Review the response to your @DELETE Streaming Distribution@ request
--     to confirm that the distribution was successfully deleted.
--
-- For information about deleting a distribution using the CloudFront
-- console, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html Deleting a Distribution>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.DeleteStreamingDistribution
  ( -- * Creating a Request
    DeleteStreamingDistribution (..),
    newDeleteStreamingDistribution,

    -- * Request Lenses
    deleteStreamingDistribution_ifMatch,
    deleteStreamingDistribution_id,

    -- * Destructuring the Response
    DeleteStreamingDistributionResponse (..),
    newDeleteStreamingDistributionResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to delete a streaming distribution.
--
-- /See:/ 'newDeleteStreamingDistribution' smart constructor.
data DeleteStreamingDistribution = DeleteStreamingDistribution'
  { -- | The value of the @ETag@ header that you received when you disabled the
    -- streaming distribution. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The distribution ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamingDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteStreamingDistribution_ifMatch' - The value of the @ETag@ header that you received when you disabled the
-- streaming distribution. For example: @E2QWRUHAPOMQZL@.
--
-- 'id', 'deleteStreamingDistribution_id' - The distribution ID.
newDeleteStreamingDistribution ::
  -- | 'id'
  Prelude.Text ->
  DeleteStreamingDistribution
newDeleteStreamingDistribution pId_ =
  DeleteStreamingDistribution'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when you disabled the
-- streaming distribution. For example: @E2QWRUHAPOMQZL@.
deleteStreamingDistribution_ifMatch :: Lens.Lens' DeleteStreamingDistribution (Prelude.Maybe Prelude.Text)
deleteStreamingDistribution_ifMatch = Lens.lens (\DeleteStreamingDistribution' {ifMatch} -> ifMatch) (\s@DeleteStreamingDistribution' {} a -> s {ifMatch = a} :: DeleteStreamingDistribution)

-- | The distribution ID.
deleteStreamingDistribution_id :: Lens.Lens' DeleteStreamingDistribution Prelude.Text
deleteStreamingDistribution_id = Lens.lens (\DeleteStreamingDistribution' {id} -> id) (\s@DeleteStreamingDistribution' {} a -> s {id = a} :: DeleteStreamingDistribution)

instance
  Prelude.AWSRequest
    DeleteStreamingDistribution
  where
  type
    Rs DeleteStreamingDistribution =
      DeleteStreamingDistributionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteStreamingDistributionResponse'

instance Prelude.Hashable DeleteStreamingDistribution

instance Prelude.NFData DeleteStreamingDistribution

instance
  Prelude.ToHeaders
    DeleteStreamingDistribution
  where
  toHeaders DeleteStreamingDistribution' {..} =
    Prelude.mconcat ["If-Match" Prelude.=# ifMatch]

instance Prelude.ToPath DeleteStreamingDistribution where
  toPath DeleteStreamingDistribution' {..} =
    Prelude.mconcat
      [ "/2020-05-31/streaming-distribution/",
        Prelude.toBS id
      ]

instance Prelude.ToQuery DeleteStreamingDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamingDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStreamingDistributionResponse ::
  DeleteStreamingDistributionResponse
newDeleteStreamingDistributionResponse =
  DeleteStreamingDistributionResponse'

instance
  Prelude.NFData
    DeleteStreamingDistributionResponse
