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
-- Module      : Network.AWS.SDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs multiple DeleteAttributes operations in a single call, which
-- reduces round trips and latencies. This enables Amazon SimpleDB to
-- optimize requests, which generally yields better throughput.
--
-- The following limitations are enforced for this operation:
--
-- -   1 MB request size
-- -   25 item limit per BatchDeleteAttributes operation
module Network.AWS.SDB.BatchDeleteAttributes
  ( -- * Creating a Request
    BatchDeleteAttributes (..),
    newBatchDeleteAttributes,

    -- * Request Lenses
    batchDeleteAttributes_domainName,
    batchDeleteAttributes_items,

    -- * Destructuring the Response
    BatchDeleteAttributesResponse (..),
    newBatchDeleteAttributesResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newBatchDeleteAttributes' smart constructor.
data BatchDeleteAttributes = BatchDeleteAttributes'
  { -- | The name of the domain in which the attributes are being deleted.
    domainName :: Core.Text,
    -- | A list of items on which to perform the operation.
    items :: [DeletableItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'batchDeleteAttributes_domainName' - The name of the domain in which the attributes are being deleted.
--
-- 'items', 'batchDeleteAttributes_items' - A list of items on which to perform the operation.
newBatchDeleteAttributes ::
  -- | 'domainName'
  Core.Text ->
  BatchDeleteAttributes
newBatchDeleteAttributes pDomainName_ =
  BatchDeleteAttributes'
    { domainName = pDomainName_,
      items = Core.mempty
    }

-- | The name of the domain in which the attributes are being deleted.
batchDeleteAttributes_domainName :: Lens.Lens' BatchDeleteAttributes Core.Text
batchDeleteAttributes_domainName = Lens.lens (\BatchDeleteAttributes' {domainName} -> domainName) (\s@BatchDeleteAttributes' {} a -> s {domainName = a} :: BatchDeleteAttributes)

-- | A list of items on which to perform the operation.
batchDeleteAttributes_items :: Lens.Lens' BatchDeleteAttributes [DeletableItem]
batchDeleteAttributes_items = Lens.lens (\BatchDeleteAttributes' {items} -> items) (\s@BatchDeleteAttributes' {} a -> s {items = a} :: BatchDeleteAttributes) Core.. Lens._Coerce

instance Core.AWSRequest BatchDeleteAttributes where
  type
    AWSResponse BatchDeleteAttributes =
      BatchDeleteAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull BatchDeleteAttributesResponse'

instance Core.Hashable BatchDeleteAttributes

instance Core.NFData BatchDeleteAttributes

instance Core.ToHeaders BatchDeleteAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BatchDeleteAttributes where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeleteAttributes where
  toQuery BatchDeleteAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BatchDeleteAttributes" :: Core.ByteString),
        "Version" Core.=: ("2009-04-15" :: Core.ByteString),
        "DomainName" Core.=: domainName,
        Core.toQueryList "Item" items
      ]

-- | /See:/ 'newBatchDeleteAttributesResponse' smart constructor.
data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchDeleteAttributesResponse ::
  BatchDeleteAttributesResponse
newBatchDeleteAttributesResponse =
  BatchDeleteAttributesResponse'

instance Core.NFData BatchDeleteAttributesResponse
