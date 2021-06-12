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
-- Module      : Network.AWS.CloudWatchEvents.DeletePartnerEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation is used by SaaS partners to delete a partner event
-- source. This operation is not used by AWS customers.
--
-- When you delete an event source, the status of the corresponding partner
-- event bus in the AWS customer account becomes DELETED.
module Network.AWS.CloudWatchEvents.DeletePartnerEventSource
  ( -- * Creating a Request
    DeletePartnerEventSource (..),
    newDeletePartnerEventSource,

    -- * Request Lenses
    deletePartnerEventSource_name,
    deletePartnerEventSource_account,

    -- * Destructuring the Response
    DeletePartnerEventSourceResponse (..),
    newDeletePartnerEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePartnerEventSource' smart constructor.
data DeletePartnerEventSource = DeletePartnerEventSource'
  { -- | The name of the event source to delete.
    name :: Core.Text,
    -- | The AWS account ID of the AWS customer that the event source was created
    -- for.
    account :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePartnerEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deletePartnerEventSource_name' - The name of the event source to delete.
--
-- 'account', 'deletePartnerEventSource_account' - The AWS account ID of the AWS customer that the event source was created
-- for.
newDeletePartnerEventSource ::
  -- | 'name'
  Core.Text ->
  -- | 'account'
  Core.Text ->
  DeletePartnerEventSource
newDeletePartnerEventSource pName_ pAccount_ =
  DeletePartnerEventSource'
    { name = pName_,
      account = pAccount_
    }

-- | The name of the event source to delete.
deletePartnerEventSource_name :: Lens.Lens' DeletePartnerEventSource Core.Text
deletePartnerEventSource_name = Lens.lens (\DeletePartnerEventSource' {name} -> name) (\s@DeletePartnerEventSource' {} a -> s {name = a} :: DeletePartnerEventSource)

-- | The AWS account ID of the AWS customer that the event source was created
-- for.
deletePartnerEventSource_account :: Lens.Lens' DeletePartnerEventSource Core.Text
deletePartnerEventSource_account = Lens.lens (\DeletePartnerEventSource' {account} -> account) (\s@DeletePartnerEventSource' {} a -> s {account = a} :: DeletePartnerEventSource)

instance Core.AWSRequest DeletePartnerEventSource where
  type
    AWSResponse DeletePartnerEventSource =
      DeletePartnerEventSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeletePartnerEventSourceResponse'

instance Core.Hashable DeletePartnerEventSource

instance Core.NFData DeletePartnerEventSource

instance Core.ToHeaders DeletePartnerEventSource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.DeletePartnerEventSource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePartnerEventSource where
  toJSON DeletePartnerEventSource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Account" Core..= account)
          ]
      )

instance Core.ToPath DeletePartnerEventSource where
  toPath = Core.const "/"

instance Core.ToQuery DeletePartnerEventSource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePartnerEventSourceResponse' smart constructor.
data DeletePartnerEventSourceResponse = DeletePartnerEventSourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePartnerEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePartnerEventSourceResponse ::
  DeletePartnerEventSourceResponse
newDeletePartnerEventSourceResponse =
  DeletePartnerEventSourceResponse'

instance Core.NFData DeletePartnerEventSourceResponse
