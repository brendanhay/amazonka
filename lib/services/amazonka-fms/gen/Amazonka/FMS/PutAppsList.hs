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
-- Module      : Amazonka.FMS.PutAppsList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Firewall Manager applications list.
module Amazonka.FMS.PutAppsList
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAppsListResponse'
            Prelude.<$> (x Data..?> "AppsList")
            Prelude.<*> (x Data..?> "AppsListArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAppsList where
  hashWithSalt _salt PutAppsList' {..} =
    _salt `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` appsList

instance Prelude.NFData PutAppsList where
  rnf PutAppsList' {..} =
    Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf appsList

instance Data.ToHeaders PutAppsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.PutAppsList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAppsList where
  toJSON PutAppsList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TagList" Data..=) Prelude.<$> tagList,
            Prelude.Just ("AppsList" Data..= appsList)
          ]
      )

instance Data.ToPath PutAppsList where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAppsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppsListResponse' smart constructor.
data PutAppsListResponse = PutAppsListResponse'
  { -- | The details of the Firewall Manager applications list.
    appsList :: Prelude.Maybe AppsListData,
    -- | The Amazon Resource Name (ARN) of the applications list.
    appsListArn :: Prelude.Maybe Prelude.Text,
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
-- 'appsList', 'putAppsListResponse_appsList' - The details of the Firewall Manager applications list.
--
-- 'appsListArn', 'putAppsListResponse_appsListArn' - The Amazon Resource Name (ARN) of the applications list.
--
-- 'httpStatus', 'putAppsListResponse_httpStatus' - The response's http status code.
newPutAppsListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAppsListResponse
newPutAppsListResponse pHttpStatus_ =
  PutAppsListResponse'
    { appsList = Prelude.Nothing,
      appsListArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the Firewall Manager applications list.
putAppsListResponse_appsList :: Lens.Lens' PutAppsListResponse (Prelude.Maybe AppsListData)
putAppsListResponse_appsList = Lens.lens (\PutAppsListResponse' {appsList} -> appsList) (\s@PutAppsListResponse' {} a -> s {appsList = a} :: PutAppsListResponse)

-- | The Amazon Resource Name (ARN) of the applications list.
putAppsListResponse_appsListArn :: Lens.Lens' PutAppsListResponse (Prelude.Maybe Prelude.Text)
putAppsListResponse_appsListArn = Lens.lens (\PutAppsListResponse' {appsListArn} -> appsListArn) (\s@PutAppsListResponse' {} a -> s {appsListArn = a} :: PutAppsListResponse)

-- | The response's http status code.
putAppsListResponse_httpStatus :: Lens.Lens' PutAppsListResponse Prelude.Int
putAppsListResponse_httpStatus = Lens.lens (\PutAppsListResponse' {httpStatus} -> httpStatus) (\s@PutAppsListResponse' {} a -> s {httpStatus = a} :: PutAppsListResponse)

instance Prelude.NFData PutAppsListResponse where
  rnf PutAppsListResponse' {..} =
    Prelude.rnf appsList
      `Prelude.seq` Prelude.rnf appsListArn
      `Prelude.seq` Prelude.rnf httpStatus
