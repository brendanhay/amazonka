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
-- Module      : Network.AWS.FMS.PutAppsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager applications list.
module Network.AWS.FMS.PutAppsList
  ( -- * Creating a Request
    PutAppsList (..),
    newPutAppsList,

    -- * Request Lenses
    putAppsList_tagList,
    putAppsList_appsList,

    -- * Destructuring the Response
    PutAppsListResponse (..),
    newPutAppsListResponse,

    -- * Response Lenses
    putAppsListResponse_appsList,
    putAppsListResponse_appsListArn,
    putAppsListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAppsList' smart constructor.
data PutAppsList = PutAppsList'
  { -- | The tags associated with the resource.
    tagList :: Core.Maybe [Tag],
    -- | The details of the AWS Firewall Manager applications list to be created.
    appsList :: AppsListData
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'putAppsList_tagList' - The tags associated with the resource.
--
-- 'appsList', 'putAppsList_appsList' - The details of the AWS Firewall Manager applications list to be created.
newPutAppsList ::
  -- | 'appsList'
  AppsListData ->
  PutAppsList
newPutAppsList pAppsList_ =
  PutAppsList'
    { tagList = Core.Nothing,
      appsList = pAppsList_
    }

-- | The tags associated with the resource.
putAppsList_tagList :: Lens.Lens' PutAppsList (Core.Maybe [Tag])
putAppsList_tagList = Lens.lens (\PutAppsList' {tagList} -> tagList) (\s@PutAppsList' {} a -> s {tagList = a} :: PutAppsList) Core.. Lens.mapping Lens._Coerce

-- | The details of the AWS Firewall Manager applications list to be created.
putAppsList_appsList :: Lens.Lens' PutAppsList AppsListData
putAppsList_appsList = Lens.lens (\PutAppsList' {appsList} -> appsList) (\s@PutAppsList' {} a -> s {appsList = a} :: PutAppsList)

instance Core.AWSRequest PutAppsList where
  type AWSResponse PutAppsList = PutAppsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAppsListResponse'
            Core.<$> (x Core..?> "AppsList")
            Core.<*> (x Core..?> "AppsListArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAppsList

instance Core.NFData PutAppsList

instance Core.ToHeaders PutAppsList where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSFMS_20180101.PutAppsList" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAppsList where
  toJSON PutAppsList' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagList" Core..=) Core.<$> tagList,
            Core.Just ("AppsList" Core..= appsList)
          ]
      )

instance Core.ToPath PutAppsList where
  toPath = Core.const "/"

instance Core.ToQuery PutAppsList where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAppsListResponse' smart constructor.
data PutAppsListResponse = PutAppsListResponse'
  { -- | The details of the AWS Firewall Manager applications list.
    appsList :: Core.Maybe AppsListData,
    -- | The Amazon Resource Name (ARN) of the applications list.
    appsListArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appsList', 'putAppsListResponse_appsList' - The details of the AWS Firewall Manager applications list.
--
-- 'appsListArn', 'putAppsListResponse_appsListArn' - The Amazon Resource Name (ARN) of the applications list.
--
-- 'httpStatus', 'putAppsListResponse_httpStatus' - The response's http status code.
newPutAppsListResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutAppsListResponse
newPutAppsListResponse pHttpStatus_ =
  PutAppsListResponse'
    { appsList = Core.Nothing,
      appsListArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the AWS Firewall Manager applications list.
putAppsListResponse_appsList :: Lens.Lens' PutAppsListResponse (Core.Maybe AppsListData)
putAppsListResponse_appsList = Lens.lens (\PutAppsListResponse' {appsList} -> appsList) (\s@PutAppsListResponse' {} a -> s {appsList = a} :: PutAppsListResponse)

-- | The Amazon Resource Name (ARN) of the applications list.
putAppsListResponse_appsListArn :: Lens.Lens' PutAppsListResponse (Core.Maybe Core.Text)
putAppsListResponse_appsListArn = Lens.lens (\PutAppsListResponse' {appsListArn} -> appsListArn) (\s@PutAppsListResponse' {} a -> s {appsListArn = a} :: PutAppsListResponse)

-- | The response's http status code.
putAppsListResponse_httpStatus :: Lens.Lens' PutAppsListResponse Core.Int
putAppsListResponse_httpStatus = Lens.lens (\PutAppsListResponse' {httpStatus} -> httpStatus) (\s@PutAppsListResponse' {} a -> s {httpStatus = a} :: PutAppsListResponse)

instance Core.NFData PutAppsListResponse
