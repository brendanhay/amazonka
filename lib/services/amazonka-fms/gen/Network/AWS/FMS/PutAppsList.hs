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
-- Creates an Firewall Manager applications list.
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
    putAppsListResponse_appsListArn,
    putAppsListResponse_appsList,
    putAppsListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAppsList' smart constructor.
data PutAppsList = PutAppsList'
  { -- | The tags associated with the resource.
    tagList :: Prelude.Maybe [Tag],
    -- | The details of the Firewall Manager applications list to be created.
    appsList :: AppsListData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'appsList', 'putAppsList_appsList' - The details of the Firewall Manager applications list to be created.
newPutAppsList ::
  -- | 'appsList'
  AppsListData ->
  PutAppsList
newPutAppsList pAppsList_ =
  PutAppsList'
    { tagList = Prelude.Nothing,
      appsList = pAppsList_
    }

-- | The tags associated with the resource.
putAppsList_tagList :: Lens.Lens' PutAppsList (Prelude.Maybe [Tag])
putAppsList_tagList = Lens.lens (\PutAppsList' {tagList} -> tagList) (\s@PutAppsList' {} a -> s {tagList = a} :: PutAppsList) Prelude.. Lens.mapping Lens.coerced

-- | The details of the Firewall Manager applications list to be created.
putAppsList_appsList :: Lens.Lens' PutAppsList AppsListData
putAppsList_appsList = Lens.lens (\PutAppsList' {appsList} -> appsList) (\s@PutAppsList' {} a -> s {appsList = a} :: PutAppsList)

instance Core.AWSRequest PutAppsList where
  type AWSResponse PutAppsList = PutAppsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAppsListResponse'
            Prelude.<$> (x Core..?> "AppsListArn")
            Prelude.<*> (x Core..?> "AppsList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAppsList

instance Prelude.NFData PutAppsList

instance Core.ToHeaders PutAppsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.PutAppsList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAppsList where
  toJSON PutAppsList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagList" Core..=) Prelude.<$> tagList,
            Prelude.Just ("AppsList" Core..= appsList)
          ]
      )

instance Core.ToPath PutAppsList where
  toPath = Prelude.const "/"

instance Core.ToQuery PutAppsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppsListResponse' smart constructor.
data PutAppsListResponse = PutAppsListResponse'
  { -- | The Amazon Resource Name (ARN) of the applications list.
    appsListArn :: Prelude.Maybe Prelude.Text,
    -- | The details of the Firewall Manager applications list.
    appsList :: Prelude.Maybe AppsListData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appsListArn', 'putAppsListResponse_appsListArn' - The Amazon Resource Name (ARN) of the applications list.
--
-- 'appsList', 'putAppsListResponse_appsList' - The details of the Firewall Manager applications list.
--
-- 'httpStatus', 'putAppsListResponse_httpStatus' - The response's http status code.
newPutAppsListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAppsListResponse
newPutAppsListResponse pHttpStatus_ =
  PutAppsListResponse'
    { appsListArn = Prelude.Nothing,
      appsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the applications list.
putAppsListResponse_appsListArn :: Lens.Lens' PutAppsListResponse (Prelude.Maybe Prelude.Text)
putAppsListResponse_appsListArn = Lens.lens (\PutAppsListResponse' {appsListArn} -> appsListArn) (\s@PutAppsListResponse' {} a -> s {appsListArn = a} :: PutAppsListResponse)

-- | The details of the Firewall Manager applications list.
putAppsListResponse_appsList :: Lens.Lens' PutAppsListResponse (Prelude.Maybe AppsListData)
putAppsListResponse_appsList = Lens.lens (\PutAppsListResponse' {appsList} -> appsList) (\s@PutAppsListResponse' {} a -> s {appsList = a} :: PutAppsListResponse)

-- | The response's http status code.
putAppsListResponse_httpStatus :: Lens.Lens' PutAppsListResponse Prelude.Int
putAppsListResponse_httpStatus = Lens.lens (\PutAppsListResponse' {httpStatus} -> httpStatus) (\s@PutAppsListResponse' {} a -> s {httpStatus = a} :: PutAppsListResponse)

instance Prelude.NFData PutAppsListResponse
