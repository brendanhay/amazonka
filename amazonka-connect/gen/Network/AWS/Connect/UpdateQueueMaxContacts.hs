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
-- Module      : Network.AWS.Connect.UpdateQueueMaxContacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the maximum number of contacts allowed in a queue before it is
-- considered full.
module Network.AWS.Connect.UpdateQueueMaxContacts
  ( -- * Creating a Request
    UpdateQueueMaxContacts (..),
    newUpdateQueueMaxContacts,

    -- * Request Lenses
    updateQueueMaxContacts_instanceId,
    updateQueueMaxContacts_queueId,
    updateQueueMaxContacts_maxContacts,

    -- * Destructuring the Response
    UpdateQueueMaxContactsResponse (..),
    newUpdateQueueMaxContactsResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueMaxContacts' smart constructor.
data UpdateQueueMaxContacts = UpdateQueueMaxContacts'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueMaxContacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQueueMaxContacts_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'updateQueueMaxContacts_queueId' - The identifier for the queue.
--
-- 'maxContacts', 'updateQueueMaxContacts_maxContacts' - The maximum number of contacts that can be in the queue before it is
-- considered full.
newUpdateQueueMaxContacts ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  -- | 'maxContacts'
  Prelude.Natural ->
  UpdateQueueMaxContacts
newUpdateQueueMaxContacts
  pInstanceId_
  pQueueId_
  pMaxContacts_ =
    UpdateQueueMaxContacts'
      { instanceId = pInstanceId_,
        queueId = pQueueId_,
        maxContacts = pMaxContacts_
      }

-- | The identifier of the Amazon Connect instance.
updateQueueMaxContacts_instanceId :: Lens.Lens' UpdateQueueMaxContacts Prelude.Text
updateQueueMaxContacts_instanceId = Lens.lens (\UpdateQueueMaxContacts' {instanceId} -> instanceId) (\s@UpdateQueueMaxContacts' {} a -> s {instanceId = a} :: UpdateQueueMaxContacts)

-- | The identifier for the queue.
updateQueueMaxContacts_queueId :: Lens.Lens' UpdateQueueMaxContacts Prelude.Text
updateQueueMaxContacts_queueId = Lens.lens (\UpdateQueueMaxContacts' {queueId} -> queueId) (\s@UpdateQueueMaxContacts' {} a -> s {queueId = a} :: UpdateQueueMaxContacts)

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
updateQueueMaxContacts_maxContacts :: Lens.Lens' UpdateQueueMaxContacts Prelude.Natural
updateQueueMaxContacts_maxContacts = Lens.lens (\UpdateQueueMaxContacts' {maxContacts} -> maxContacts) (\s@UpdateQueueMaxContacts' {} a -> s {maxContacts = a} :: UpdateQueueMaxContacts)

instance Prelude.AWSRequest UpdateQueueMaxContacts where
  type
    Rs UpdateQueueMaxContacts =
      UpdateQueueMaxContactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQueueMaxContactsResponse'

instance Prelude.Hashable UpdateQueueMaxContacts

instance Prelude.NFData UpdateQueueMaxContacts

instance Prelude.ToHeaders UpdateQueueMaxContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateQueueMaxContacts where
  toJSON UpdateQueueMaxContacts' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MaxContacts" Prelude..= maxContacts)
          ]
      )

instance Prelude.ToPath UpdateQueueMaxContacts where
  toPath UpdateQueueMaxContacts' {..} =
    Prelude.mconcat
      [ "/queues/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS queueId,
        "/max-contacts"
      ]

instance Prelude.ToQuery UpdateQueueMaxContacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueMaxContactsResponse' smart constructor.
data UpdateQueueMaxContactsResponse = UpdateQueueMaxContactsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueMaxContactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueMaxContactsResponse ::
  UpdateQueueMaxContactsResponse
newUpdateQueueMaxContactsResponse =
  UpdateQueueMaxContactsResponse'

instance
  Prelude.NFData
    UpdateQueueMaxContactsResponse
