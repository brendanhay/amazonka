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
-- Module      : Network.AWS.FMS.PutProtocolsList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager protocols list.
module Network.AWS.FMS.PutProtocolsList
  ( -- * Creating a Request
    PutProtocolsList (..),
    newPutProtocolsList,

    -- * Request Lenses
    putProtocolsList_tagList,
    putProtocolsList_protocolsList,

    -- * Destructuring the Response
    PutProtocolsListResponse (..),
    newPutProtocolsListResponse,

    -- * Response Lenses
    putProtocolsListResponse_protocolsList,
    putProtocolsListResponse_protocolsListArn,
    putProtocolsListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutProtocolsList' smart constructor.
data PutProtocolsList = PutProtocolsList'
  { -- | The tags associated with the resource.
    tagList :: Core.Maybe [Tag],
    -- | The details of the AWS Firewall Manager protocols list to be created.
    protocolsList :: ProtocolsListData
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutProtocolsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'putProtocolsList_tagList' - The tags associated with the resource.
--
-- 'protocolsList', 'putProtocolsList_protocolsList' - The details of the AWS Firewall Manager protocols list to be created.
newPutProtocolsList ::
  -- | 'protocolsList'
  ProtocolsListData ->
  PutProtocolsList
newPutProtocolsList pProtocolsList_ =
  PutProtocolsList'
    { tagList = Core.Nothing,
      protocolsList = pProtocolsList_
    }

-- | The tags associated with the resource.
putProtocolsList_tagList :: Lens.Lens' PutProtocolsList (Core.Maybe [Tag])
putProtocolsList_tagList = Lens.lens (\PutProtocolsList' {tagList} -> tagList) (\s@PutProtocolsList' {} a -> s {tagList = a} :: PutProtocolsList) Core.. Lens.mapping Lens._Coerce

-- | The details of the AWS Firewall Manager protocols list to be created.
putProtocolsList_protocolsList :: Lens.Lens' PutProtocolsList ProtocolsListData
putProtocolsList_protocolsList = Lens.lens (\PutProtocolsList' {protocolsList} -> protocolsList) (\s@PutProtocolsList' {} a -> s {protocolsList = a} :: PutProtocolsList)

instance Core.AWSRequest PutProtocolsList where
  type
    AWSResponse PutProtocolsList =
      PutProtocolsListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutProtocolsListResponse'
            Core.<$> (x Core..?> "ProtocolsList")
            Core.<*> (x Core..?> "ProtocolsListArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutProtocolsList

instance Core.NFData PutProtocolsList

instance Core.ToHeaders PutProtocolsList where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.PutProtocolsList" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutProtocolsList where
  toJSON PutProtocolsList' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagList" Core..=) Core.<$> tagList,
            Core.Just ("ProtocolsList" Core..= protocolsList)
          ]
      )

instance Core.ToPath PutProtocolsList where
  toPath = Core.const "/"

instance Core.ToQuery PutProtocolsList where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutProtocolsListResponse' smart constructor.
data PutProtocolsListResponse = PutProtocolsListResponse'
  { -- | The details of the AWS Firewall Manager protocols list.
    protocolsList :: Core.Maybe ProtocolsListData,
    -- | The Amazon Resource Name (ARN) of the protocols list.
    protocolsListArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutProtocolsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocolsList', 'putProtocolsListResponse_protocolsList' - The details of the AWS Firewall Manager protocols list.
--
-- 'protocolsListArn', 'putProtocolsListResponse_protocolsListArn' - The Amazon Resource Name (ARN) of the protocols list.
--
-- 'httpStatus', 'putProtocolsListResponse_httpStatus' - The response's http status code.
newPutProtocolsListResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutProtocolsListResponse
newPutProtocolsListResponse pHttpStatus_ =
  PutProtocolsListResponse'
    { protocolsList =
        Core.Nothing,
      protocolsListArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the AWS Firewall Manager protocols list.
putProtocolsListResponse_protocolsList :: Lens.Lens' PutProtocolsListResponse (Core.Maybe ProtocolsListData)
putProtocolsListResponse_protocolsList = Lens.lens (\PutProtocolsListResponse' {protocolsList} -> protocolsList) (\s@PutProtocolsListResponse' {} a -> s {protocolsList = a} :: PutProtocolsListResponse)

-- | The Amazon Resource Name (ARN) of the protocols list.
putProtocolsListResponse_protocolsListArn :: Lens.Lens' PutProtocolsListResponse (Core.Maybe Core.Text)
putProtocolsListResponse_protocolsListArn = Lens.lens (\PutProtocolsListResponse' {protocolsListArn} -> protocolsListArn) (\s@PutProtocolsListResponse' {} a -> s {protocolsListArn = a} :: PutProtocolsListResponse)

-- | The response's http status code.
putProtocolsListResponse_httpStatus :: Lens.Lens' PutProtocolsListResponse Core.Int
putProtocolsListResponse_httpStatus = Lens.lens (\PutProtocolsListResponse' {httpStatus} -> httpStatus) (\s@PutProtocolsListResponse' {} a -> s {httpStatus = a} :: PutProtocolsListResponse)

instance Core.NFData PutProtocolsListResponse
