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
-- Module      : Network.AWS.FMS.GetAppsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager
-- applications list.
module Network.AWS.FMS.GetAppsList
  ( -- * Creating a Request
    GetAppsList (..),
    newGetAppsList,

    -- * Request Lenses
    getAppsList_defaultList,
    getAppsList_listId,

    -- * Destructuring the Response
    GetAppsListResponse (..),
    newGetAppsListResponse,

    -- * Response Lenses
    getAppsListResponse_appsList,
    getAppsListResponse_appsListArn,
    getAppsListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAppsList' smart constructor.
data GetAppsList = GetAppsList'
  { -- | Specifies whether the list to retrieve is a default list owned by AWS
    -- Firewall Manager.
    defaultList :: Core.Maybe Core.Bool,
    -- | The ID of the AWS Firewall Manager applications list that you want the
    -- details for.
    listId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultList', 'getAppsList_defaultList' - Specifies whether the list to retrieve is a default list owned by AWS
-- Firewall Manager.
--
-- 'listId', 'getAppsList_listId' - The ID of the AWS Firewall Manager applications list that you want the
-- details for.
newGetAppsList ::
  -- | 'listId'
  Core.Text ->
  GetAppsList
newGetAppsList pListId_ =
  GetAppsList'
    { defaultList = Core.Nothing,
      listId = pListId_
    }

-- | Specifies whether the list to retrieve is a default list owned by AWS
-- Firewall Manager.
getAppsList_defaultList :: Lens.Lens' GetAppsList (Core.Maybe Core.Bool)
getAppsList_defaultList = Lens.lens (\GetAppsList' {defaultList} -> defaultList) (\s@GetAppsList' {} a -> s {defaultList = a} :: GetAppsList)

-- | The ID of the AWS Firewall Manager applications list that you want the
-- details for.
getAppsList_listId :: Lens.Lens' GetAppsList Core.Text
getAppsList_listId = Lens.lens (\GetAppsList' {listId} -> listId) (\s@GetAppsList' {} a -> s {listId = a} :: GetAppsList)

instance Core.AWSRequest GetAppsList where
  type AWSResponse GetAppsList = GetAppsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppsListResponse'
            Core.<$> (x Core..?> "AppsList")
            Core.<*> (x Core..?> "AppsListArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAppsList

instance Core.NFData GetAppsList

instance Core.ToHeaders GetAppsList where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSFMS_20180101.GetAppsList" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAppsList where
  toJSON GetAppsList' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultList" Core..=) Core.<$> defaultList,
            Core.Just ("ListId" Core..= listId)
          ]
      )

instance Core.ToPath GetAppsList where
  toPath = Core.const "/"

instance Core.ToQuery GetAppsList where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAppsListResponse' smart constructor.
data GetAppsListResponse = GetAppsListResponse'
  { -- | Information about the specified AWS Firewall Manager applications list.
    appsList :: Core.Maybe AppsListData,
    -- | The Amazon Resource Name (ARN) of the applications list.
    appsListArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appsList', 'getAppsListResponse_appsList' - Information about the specified AWS Firewall Manager applications list.
--
-- 'appsListArn', 'getAppsListResponse_appsListArn' - The Amazon Resource Name (ARN) of the applications list.
--
-- 'httpStatus', 'getAppsListResponse_httpStatus' - The response's http status code.
newGetAppsListResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAppsListResponse
newGetAppsListResponse pHttpStatus_ =
  GetAppsListResponse'
    { appsList = Core.Nothing,
      appsListArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified AWS Firewall Manager applications list.
getAppsListResponse_appsList :: Lens.Lens' GetAppsListResponse (Core.Maybe AppsListData)
getAppsListResponse_appsList = Lens.lens (\GetAppsListResponse' {appsList} -> appsList) (\s@GetAppsListResponse' {} a -> s {appsList = a} :: GetAppsListResponse)

-- | The Amazon Resource Name (ARN) of the applications list.
getAppsListResponse_appsListArn :: Lens.Lens' GetAppsListResponse (Core.Maybe Core.Text)
getAppsListResponse_appsListArn = Lens.lens (\GetAppsListResponse' {appsListArn} -> appsListArn) (\s@GetAppsListResponse' {} a -> s {appsListArn = a} :: GetAppsListResponse)

-- | The response's http status code.
getAppsListResponse_httpStatus :: Lens.Lens' GetAppsListResponse Core.Int
getAppsListResponse_httpStatus = Lens.lens (\GetAppsListResponse' {httpStatus} -> httpStatus) (\s@GetAppsListResponse' {} a -> s {httpStatus = a} :: GetAppsListResponse)

instance Core.NFData GetAppsListResponse
