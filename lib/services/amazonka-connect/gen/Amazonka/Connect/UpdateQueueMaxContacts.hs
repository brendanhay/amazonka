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
-- Module      : Amazonka.Connect.UpdateQueueMaxContacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the maximum number of contacts allowed in a queue before it is
-- considered full.
module Amazonka.Connect.UpdateQueueMaxContacts
  ( -- * Creating a Request
    UpdateQueueMaxContacts (..),
    newUpdateQueueMaxContacts,

    -- * Request Lenses
    updateQueueMaxContacts_maxContacts,
    updateQueueMaxContacts_instanceId,
    updateQueueMaxContacts_queueId,

    -- * Destructuring the Response
    UpdateQueueMaxContactsResponse (..),
    newUpdateQueueMaxContactsResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQueueMaxContacts' smart constructor.
data UpdateQueueMaxContacts = UpdateQueueMaxContacts'
  { -- | The maximum number of contacts that can be in the queue before it is
    -- considered full.
    maxContacts :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueMaxContacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxContacts', 'updateQueueMaxContacts_maxContacts' - The maximum number of contacts that can be in the queue before it is
-- considered full.
--
-- 'instanceId', 'updateQueueMaxContacts_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'queueId', 'updateQueueMaxContacts_queueId' - The identifier for the queue.
newUpdateQueueMaxContacts ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  UpdateQueueMaxContacts
newUpdateQueueMaxContacts pInstanceId_ pQueueId_ =
  UpdateQueueMaxContacts'
    { maxContacts =
        Prelude.Nothing,
      instanceId = pInstanceId_,
      queueId = pQueueId_
    }

-- | The maximum number of contacts that can be in the queue before it is
-- considered full.
updateQueueMaxContacts_maxContacts :: Lens.Lens' UpdateQueueMaxContacts (Prelude.Maybe Prelude.Natural)
updateQueueMaxContacts_maxContacts = Lens.lens (\UpdateQueueMaxContacts' {maxContacts} -> maxContacts) (\s@UpdateQueueMaxContacts' {} a -> s {maxContacts = a} :: UpdateQueueMaxContacts)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateQueueMaxContacts_instanceId :: Lens.Lens' UpdateQueueMaxContacts Prelude.Text
updateQueueMaxContacts_instanceId = Lens.lens (\UpdateQueueMaxContacts' {instanceId} -> instanceId) (\s@UpdateQueueMaxContacts' {} a -> s {instanceId = a} :: UpdateQueueMaxContacts)

-- | The identifier for the queue.
updateQueueMaxContacts_queueId :: Lens.Lens' UpdateQueueMaxContacts Prelude.Text
updateQueueMaxContacts_queueId = Lens.lens (\UpdateQueueMaxContacts' {queueId} -> queueId) (\s@UpdateQueueMaxContacts' {} a -> s {queueId = a} :: UpdateQueueMaxContacts)

instance Core.AWSRequest UpdateQueueMaxContacts where
  type
    AWSResponse UpdateQueueMaxContacts =
      UpdateQueueMaxContactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateQueueMaxContactsResponse'

instance Prelude.Hashable UpdateQueueMaxContacts where
  hashWithSalt _salt UpdateQueueMaxContacts' {..} =
    _salt
      `Prelude.hashWithSalt` maxContacts
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` queueId

instance Prelude.NFData UpdateQueueMaxContacts where
  rnf UpdateQueueMaxContacts' {..} =
    Prelude.rnf maxContacts `Prelude.seq`
      Prelude.rnf instanceId `Prelude.seq`
        Prelude.rnf queueId

instance Data.ToHeaders UpdateQueueMaxContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateQueueMaxContacts where
  toJSON UpdateQueueMaxContacts' {..} =
    Data.object
      ( Prelude.catMaybes
          [("MaxContacts" Data..=) Prelude.<$> maxContacts]
      )

instance Data.ToPath UpdateQueueMaxContacts where
  toPath UpdateQueueMaxContacts' {..} =
    Prelude.mconcat
      [ "/queues/",
        Data.toBS instanceId,
        "/",
        Data.toBS queueId,
        "/max-contacts"
      ]

instance Data.ToQuery UpdateQueueMaxContacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueMaxContactsResponse' smart constructor.
data UpdateQueueMaxContactsResponse = UpdateQueueMaxContactsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
