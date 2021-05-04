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
-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the endpoint for a device and mobile app from Amazon SNS. This
-- action is idempotent. For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- When you delete an endpoint that is also subscribed to a topic, then you
-- must also unsubscribe the endpoint from the topic.
module Network.AWS.SNS.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_endpointArn,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for DeleteEndpoint action.
--
-- /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | EndpointArn of endpoint to delete.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointArn', 'deleteEndpoint_endpointArn' - EndpointArn of endpoint to delete.
newDeleteEndpoint ::
  -- | 'endpointArn'
  Prelude.Text ->
  DeleteEndpoint
newDeleteEndpoint pEndpointArn_ =
  DeleteEndpoint' {endpointArn = pEndpointArn_}

-- | EndpointArn of endpoint to delete.
deleteEndpoint_endpointArn :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_endpointArn = Lens.lens (\DeleteEndpoint' {endpointArn} -> endpointArn) (\s@DeleteEndpoint' {} a -> s {endpointArn = a} :: DeleteEndpoint)

instance Prelude.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteEndpointResponse'

instance Prelude.Hashable DeleteEndpoint

instance Prelude.NFData DeleteEndpoint

instance Prelude.ToHeaders DeleteEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEndpoint where
  toQuery DeleteEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteEndpoint" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "EndpointArn" Prelude.=: endpointArn
      ]

-- | /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEndpointResponse ::
  DeleteEndpointResponse
newDeleteEndpointResponse = DeleteEndpointResponse'

instance Prelude.NFData DeleteEndpointResponse
