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
-- Creates an Firewall Manager protocols list.
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutProtocolsList' smart constructor.
data PutProtocolsList = PutProtocolsList'
  { -- | The tags associated with the resource.
    tagList :: Prelude.Maybe [Tag],
    -- | The details of the Firewall Manager protocols list to be created.
    protocolsList :: ProtocolsListData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'protocolsList', 'putProtocolsList_protocolsList' - The details of the Firewall Manager protocols list to be created.
newPutProtocolsList ::
  -- | 'protocolsList'
  ProtocolsListData ->
  PutProtocolsList
newPutProtocolsList pProtocolsList_ =
  PutProtocolsList'
    { tagList = Prelude.Nothing,
      protocolsList = pProtocolsList_
    }

-- | The tags associated with the resource.
putProtocolsList_tagList :: Lens.Lens' PutProtocolsList (Prelude.Maybe [Tag])
putProtocolsList_tagList = Lens.lens (\PutProtocolsList' {tagList} -> tagList) (\s@PutProtocolsList' {} a -> s {tagList = a} :: PutProtocolsList) Prelude.. Lens.mapping Lens._Coerce

-- | The details of the Firewall Manager protocols list to be created.
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
            Prelude.<$> (x Core..?> "ProtocolsList")
            Prelude.<*> (x Core..?> "ProtocolsListArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutProtocolsList

instance Prelude.NFData PutProtocolsList

instance Core.ToHeaders PutProtocolsList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.PutProtocolsList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutProtocolsList where
  toJSON PutProtocolsList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagList" Core..=) Prelude.<$> tagList,
            Prelude.Just
              ("ProtocolsList" Core..= protocolsList)
          ]
      )

instance Core.ToPath PutProtocolsList where
  toPath = Prelude.const "/"

instance Core.ToQuery PutProtocolsList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutProtocolsListResponse' smart constructor.
data PutProtocolsListResponse = PutProtocolsListResponse'
  { -- | The details of the Firewall Manager protocols list.
    protocolsList :: Prelude.Maybe ProtocolsListData,
    -- | The Amazon Resource Name (ARN) of the protocols list.
    protocolsListArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProtocolsListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocolsList', 'putProtocolsListResponse_protocolsList' - The details of the Firewall Manager protocols list.
--
-- 'protocolsListArn', 'putProtocolsListResponse_protocolsListArn' - The Amazon Resource Name (ARN) of the protocols list.
--
-- 'httpStatus', 'putProtocolsListResponse_httpStatus' - The response's http status code.
newPutProtocolsListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutProtocolsListResponse
newPutProtocolsListResponse pHttpStatus_ =
  PutProtocolsListResponse'
    { protocolsList =
        Prelude.Nothing,
      protocolsListArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the Firewall Manager protocols list.
putProtocolsListResponse_protocolsList :: Lens.Lens' PutProtocolsListResponse (Prelude.Maybe ProtocolsListData)
putProtocolsListResponse_protocolsList = Lens.lens (\PutProtocolsListResponse' {protocolsList} -> protocolsList) (\s@PutProtocolsListResponse' {} a -> s {protocolsList = a} :: PutProtocolsListResponse)

-- | The Amazon Resource Name (ARN) of the protocols list.
putProtocolsListResponse_protocolsListArn :: Lens.Lens' PutProtocolsListResponse (Prelude.Maybe Prelude.Text)
putProtocolsListResponse_protocolsListArn = Lens.lens (\PutProtocolsListResponse' {protocolsListArn} -> protocolsListArn) (\s@PutProtocolsListResponse' {} a -> s {protocolsListArn = a} :: PutProtocolsListResponse)

-- | The response's http status code.
putProtocolsListResponse_httpStatus :: Lens.Lens' PutProtocolsListResponse Prelude.Int
putProtocolsListResponse_httpStatus = Lens.lens (\PutProtocolsListResponse' {httpStatus} -> httpStatus) (\s@PutProtocolsListResponse' {} a -> s {httpStatus = a} :: PutProtocolsListResponse)

instance Prelude.NFData PutProtocolsListResponse
