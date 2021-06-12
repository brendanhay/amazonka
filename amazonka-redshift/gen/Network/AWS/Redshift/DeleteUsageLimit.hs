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
-- Module      : Network.AWS.Redshift.DeleteUsageLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage limit from a cluster.
module Network.AWS.Redshift.DeleteUsageLimit
  ( -- * Creating a Request
    DeleteUsageLimit (..),
    newDeleteUsageLimit,

    -- * Request Lenses
    deleteUsageLimit_usageLimitId,

    -- * Destructuring the Response
    DeleteUsageLimitResponse (..),
    newDeleteUsageLimitResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUsageLimit' smart constructor.
data DeleteUsageLimit = DeleteUsageLimit'
  { -- | The identifier of the usage limit to delete.
    usageLimitId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimitId', 'deleteUsageLimit_usageLimitId' - The identifier of the usage limit to delete.
newDeleteUsageLimit ::
  -- | 'usageLimitId'
  Core.Text ->
  DeleteUsageLimit
newDeleteUsageLimit pUsageLimitId_ =
  DeleteUsageLimit' {usageLimitId = pUsageLimitId_}

-- | The identifier of the usage limit to delete.
deleteUsageLimit_usageLimitId :: Lens.Lens' DeleteUsageLimit Core.Text
deleteUsageLimit_usageLimitId = Lens.lens (\DeleteUsageLimit' {usageLimitId} -> usageLimitId) (\s@DeleteUsageLimit' {} a -> s {usageLimitId = a} :: DeleteUsageLimit)

instance Core.AWSRequest DeleteUsageLimit where
  type
    AWSResponse DeleteUsageLimit =
      DeleteUsageLimitResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteUsageLimitResponse'

instance Core.Hashable DeleteUsageLimit

instance Core.NFData DeleteUsageLimit

instance Core.ToHeaders DeleteUsageLimit where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteUsageLimit where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUsageLimit where
  toQuery DeleteUsageLimit' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteUsageLimit" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "UsageLimitId" Core.=: usageLimitId
      ]

-- | /See:/ 'newDeleteUsageLimitResponse' smart constructor.
data DeleteUsageLimitResponse = DeleteUsageLimitResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUsageLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsageLimitResponse ::
  DeleteUsageLimitResponse
newDeleteUsageLimitResponse =
  DeleteUsageLimitResponse'

instance Core.NFData DeleteUsageLimitResponse
