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
-- Module      : Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription filter.
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
  ( -- * Creating a Request
    DeleteSubscriptionFilter (..),
    newDeleteSubscriptionFilter,

    -- * Request Lenses
    deleteSubscriptionFilter_logGroupName,
    deleteSubscriptionFilter_filterName,

    -- * Destructuring the Response
    DeleteSubscriptionFilterResponse (..),
    newDeleteSubscriptionFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSubscriptionFilter' smart constructor.
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The name of the subscription filter.
    filterName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubscriptionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'deleteSubscriptionFilter_logGroupName' - The name of the log group.
--
-- 'filterName', 'deleteSubscriptionFilter_filterName' - The name of the subscription filter.
newDeleteSubscriptionFilter ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'filterName'
  Core.Text ->
  DeleteSubscriptionFilter
newDeleteSubscriptionFilter
  pLogGroupName_
  pFilterName_ =
    DeleteSubscriptionFilter'
      { logGroupName =
          pLogGroupName_,
        filterName = pFilterName_
      }

-- | The name of the log group.
deleteSubscriptionFilter_logGroupName :: Lens.Lens' DeleteSubscriptionFilter Core.Text
deleteSubscriptionFilter_logGroupName = Lens.lens (\DeleteSubscriptionFilter' {logGroupName} -> logGroupName) (\s@DeleteSubscriptionFilter' {} a -> s {logGroupName = a} :: DeleteSubscriptionFilter)

-- | The name of the subscription filter.
deleteSubscriptionFilter_filterName :: Lens.Lens' DeleteSubscriptionFilter Core.Text
deleteSubscriptionFilter_filterName = Lens.lens (\DeleteSubscriptionFilter' {filterName} -> filterName) (\s@DeleteSubscriptionFilter' {} a -> s {filterName = a} :: DeleteSubscriptionFilter)

instance Core.AWSRequest DeleteSubscriptionFilter where
  type
    AWSResponse DeleteSubscriptionFilter =
      DeleteSubscriptionFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteSubscriptionFilterResponse'

instance Core.Hashable DeleteSubscriptionFilter

instance Core.NFData DeleteSubscriptionFilter

instance Core.ToHeaders DeleteSubscriptionFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DeleteSubscriptionFilter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSubscriptionFilter where
  toJSON DeleteSubscriptionFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("filterName" Core..= filterName)
          ]
      )

instance Core.ToPath DeleteSubscriptionFilter where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSubscriptionFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSubscriptionFilterResponse' smart constructor.
data DeleteSubscriptionFilterResponse = DeleteSubscriptionFilterResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubscriptionFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSubscriptionFilterResponse ::
  DeleteSubscriptionFilterResponse
newDeleteSubscriptionFilterResponse =
  DeleteSubscriptionFilterResponse'

instance Core.NFData DeleteSubscriptionFilterResponse
