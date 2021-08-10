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
-- Module      : Network.AWS.FMS.GetProtocolsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager protocols
-- list.
module Network.AWS.FMS.GetProtocolsList
  ( -- * Creating a Request
    GetProtocolsList (..),
    newGetProtocolsList,

    -- * Request Lenses
    getProtocolsList_defaultList,
    getProtocolsList_listId,

    -- * Destructuring the Response
    GetProtocolsListResponse (..),
    newGetProtocolsListResponse,

    -- * Response Lenses
    getProtocolsListResponse_protocolsList,
    getProtocolsListResponse_protocolsListArn,
    getProtocolsListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetProtocolsList' smart constructor.
data GetProtocolsList = GetProtocolsList'
  { -- | Specifies whether the list to retrieve is a default list owned by AWS
    -- Firewall Manager.
    defaultList :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AWS Firewall Manager protocols list that you want the
    -- details for.
    listId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProtocolsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultList', 'getProtocolsList_defaultList' - Specifies whether the list to retrieve is a default list owned by AWS
-- Firewall Manager.
--
-- 'listId', 'getProtocolsList_listId' - The ID of the AWS Firewall Manager protocols list that you want the
-- details for.
newGetProtocolsList ::
  -- | 'listId'
  Prelude.Text ->
  GetProtocolsList
newGetProtocolsList pListId_ =
  GetProtocolsList'
    { defaultList = Prelude.Nothing,
      listId = pListId_
    }

-- | Specifies whether the list to retrieve is a default list owned by AWS
-- Firewall Manager.
getProtocolsList_defaultList :: Lens.Lens' GetProtocolsList (Prelude.Maybe Prelude.Bool)
getProtocolsList_defaultList = Lens.lens (\GetProtocolsList' {defaultList} -> defaultList) (\s@GetProtocolsList' {} a -> s {defaultList = a} :: GetProtocolsList)

-- | The ID of the AWS Firewall Manager protocols list that you want the
-- details for.
getProtocolsList_listId :: Lens.Lens' GetProtocolsList Prelude.Text
getProtocolsList_listId = Lens.lens (\GetProtocolsList' {listId} -> listId) (\s@GetProtocolsList' {} a -> s {listId = a} :: GetProtocolsList)

instance Core.AWSRequest GetProtocolsList where
  type
    AWSResponse GetProtocolsList =
      GetProtocolsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProtocolsListResponse'
            Prelude.<$> (x Core..?> "ProtocolsList")
            Prelude.<*> (x Core..?> "ProtocolsListArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProtocolsList

instance Prelude.NFData GetProtocolsList

instance Core.ToHeaders GetProtocolsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetProtocolsList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetProtocolsList where
  toJSON GetProtocolsList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultList" Core..=) Prelude.<$> defaultList,
            Prelude.Just ("ListId" Core..= listId)
          ]
      )

instance Core.ToPath GetProtocolsList where
  toPath = Prelude.const "/"

instance Core.ToQuery GetProtocolsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProtocolsListResponse' smart constructor.
data GetProtocolsListResponse = GetProtocolsListResponse'
  { -- | Information about the specified AWS Firewall Manager protocols list.
    protocolsList :: Prelude.Maybe ProtocolsListData,
    -- | The Amazon Resource Name (ARN) of the specified protocols list.
    protocolsListArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProtocolsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocolsList', 'getProtocolsListResponse_protocolsList' - Information about the specified AWS Firewall Manager protocols list.
--
-- 'protocolsListArn', 'getProtocolsListResponse_protocolsListArn' - The Amazon Resource Name (ARN) of the specified protocols list.
--
-- 'httpStatus', 'getProtocolsListResponse_httpStatus' - The response's http status code.
newGetProtocolsListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProtocolsListResponse
newGetProtocolsListResponse pHttpStatus_ =
  GetProtocolsListResponse'
    { protocolsList =
        Prelude.Nothing,
      protocolsListArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified AWS Firewall Manager protocols list.
getProtocolsListResponse_protocolsList :: Lens.Lens' GetProtocolsListResponse (Prelude.Maybe ProtocolsListData)
getProtocolsListResponse_protocolsList = Lens.lens (\GetProtocolsListResponse' {protocolsList} -> protocolsList) (\s@GetProtocolsListResponse' {} a -> s {protocolsList = a} :: GetProtocolsListResponse)

-- | The Amazon Resource Name (ARN) of the specified protocols list.
getProtocolsListResponse_protocolsListArn :: Lens.Lens' GetProtocolsListResponse (Prelude.Maybe Prelude.Text)
getProtocolsListResponse_protocolsListArn = Lens.lens (\GetProtocolsListResponse' {protocolsListArn} -> protocolsListArn) (\s@GetProtocolsListResponse' {} a -> s {protocolsListArn = a} :: GetProtocolsListResponse)

-- | The response's http status code.
getProtocolsListResponse_httpStatus :: Lens.Lens' GetProtocolsListResponse Prelude.Int
getProtocolsListResponse_httpStatus = Lens.lens (\GetProtocolsListResponse' {httpStatus} -> httpStatus) (\s@GetProtocolsListResponse' {} a -> s {httpStatus = a} :: GetProtocolsListResponse)

instance Prelude.NFData GetProtocolsListResponse
