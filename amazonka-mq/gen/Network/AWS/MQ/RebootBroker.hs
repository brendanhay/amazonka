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
-- Module      : Network.AWS.MQ.RebootBroker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a broker. Note: This API is asynchronous.
module Network.AWS.MQ.RebootBroker
  ( -- * Creating a Request
    RebootBroker (..),
    newRebootBroker,

    -- * Request Lenses
    rebootBroker_brokerId,

    -- * Destructuring the Response
    RebootBrokerResponse (..),
    newRebootBrokerResponse,

    -- * Response Lenses
    rebootBrokerResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootBroker' smart constructor.
data RebootBroker = RebootBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'rebootBroker_brokerId' - The unique ID that Amazon MQ generates for the broker.
newRebootBroker ::
  -- | 'brokerId'
  Prelude.Text ->
  RebootBroker
newRebootBroker pBrokerId_ =
  RebootBroker' {brokerId = pBrokerId_}

-- | The unique ID that Amazon MQ generates for the broker.
rebootBroker_brokerId :: Lens.Lens' RebootBroker Prelude.Text
rebootBroker_brokerId = Lens.lens (\RebootBroker' {brokerId} -> brokerId) (\s@RebootBroker' {} a -> s {brokerId = a} :: RebootBroker)

instance Prelude.AWSRequest RebootBroker where
  type Rs RebootBroker = RebootBrokerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RebootBrokerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootBroker

instance Prelude.NFData RebootBroker

instance Prelude.ToHeaders RebootBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RebootBroker where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath RebootBroker where
  toPath RebootBroker' {..} =
    Prelude.mconcat
      ["/v1/brokers/", Prelude.toBS brokerId, "/reboot"]

instance Prelude.ToQuery RebootBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootBrokerResponse' smart constructor.
data RebootBrokerResponse = RebootBrokerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rebootBrokerResponse_httpStatus' - The response's http status code.
newRebootBrokerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootBrokerResponse
newRebootBrokerResponse pHttpStatus_ =
  RebootBrokerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
rebootBrokerResponse_httpStatus :: Lens.Lens' RebootBrokerResponse Prelude.Int
rebootBrokerResponse_httpStatus = Lens.lens (\RebootBrokerResponse' {httpStatus} -> httpStatus) (\s@RebootBrokerResponse' {} a -> s {httpStatus = a} :: RebootBrokerResponse)

instance Prelude.NFData RebootBrokerResponse
