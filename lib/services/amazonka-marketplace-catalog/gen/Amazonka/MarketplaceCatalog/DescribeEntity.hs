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
-- Module      : Amazonka.MarketplaceCatalog.DescribeEntity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata and content of the entity.
module Amazonka.MarketplaceCatalog.DescribeEntity
  ( -- * Creating a Request
    DescribeEntity (..),
    newDescribeEntity,

    -- * Request Lenses
    describeEntity_catalog,
    describeEntity_entityId,

    -- * Destructuring the Response
    DescribeEntityResponse (..),
    newDescribeEntityResponse,

    -- * Response Lenses
    describeEntityResponse_lastModifiedDate,
    describeEntityResponse_details,
    describeEntityResponse_entityType,
    describeEntityResponse_entityIdentifier,
    describeEntityResponse_entityArn,
    describeEntityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEntity' smart constructor.
data DescribeEntity = DescribeEntity'
  { -- | Required. The catalog related to the request. Fixed value:
    -- @AWSMarketplace@
    catalog :: Prelude.Text,
    -- | Required. The unique ID of the entity to describe.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalog', 'describeEntity_catalog' - Required. The catalog related to the request. Fixed value:
-- @AWSMarketplace@
--
-- 'entityId', 'describeEntity_entityId' - Required. The unique ID of the entity to describe.
newDescribeEntity ::
  -- | 'catalog'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  DescribeEntity
newDescribeEntity pCatalog_ pEntityId_ =
  DescribeEntity'
    { catalog = pCatalog_,
      entityId = pEntityId_
    }

-- | Required. The catalog related to the request. Fixed value:
-- @AWSMarketplace@
describeEntity_catalog :: Lens.Lens' DescribeEntity Prelude.Text
describeEntity_catalog = Lens.lens (\DescribeEntity' {catalog} -> catalog) (\s@DescribeEntity' {} a -> s {catalog = a} :: DescribeEntity)

-- | Required. The unique ID of the entity to describe.
describeEntity_entityId :: Lens.Lens' DescribeEntity Prelude.Text
describeEntity_entityId = Lens.lens (\DescribeEntity' {entityId} -> entityId) (\s@DescribeEntity' {} a -> s {entityId = a} :: DescribeEntity)

instance Core.AWSRequest DescribeEntity where
  type
    AWSResponse DescribeEntity =
      DescribeEntityResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntityResponse'
            Prelude.<$> (x Core..?> "LastModifiedDate")
            Prelude.<*> (x Core..?> "Details")
            Prelude.<*> (x Core..?> "EntityType")
            Prelude.<*> (x Core..?> "EntityIdentifier")
            Prelude.<*> (x Core..?> "EntityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEntity where
  hashWithSalt _salt DescribeEntity' {..} =
    _salt `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData DescribeEntity where
  rnf DescribeEntity' {..} =
    Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf entityId

instance Core.ToHeaders DescribeEntity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeEntity where
  toPath = Prelude.const "/DescribeEntity"

instance Core.ToQuery DescribeEntity where
  toQuery DescribeEntity' {..} =
    Prelude.mconcat
      [ "catalog" Core.=: catalog,
        "entityId" Core.=: entityId
      ]

-- | /See:/ 'newDescribeEntityResponse' smart constructor.
data DescribeEntityResponse = DescribeEntityResponse'
  { -- | The last modified date of the entity, in ISO 8601 format
    -- (2018-02-27T13:45:22Z).
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | This stringified JSON object includes the details of the entity.
    details :: Prelude.Maybe Prelude.Text,
    -- | The named type of the entity, in the format of @EntityType\@Version@.
    entityType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the entity, in the format of @EntityId\@RevisionId@.
    entityIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ARN associated to the unique identifier for the entity referenced in
    -- this request.
    entityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'describeEntityResponse_lastModifiedDate' - The last modified date of the entity, in ISO 8601 format
-- (2018-02-27T13:45:22Z).
--
-- 'details', 'describeEntityResponse_details' - This stringified JSON object includes the details of the entity.
--
-- 'entityType', 'describeEntityResponse_entityType' - The named type of the entity, in the format of @EntityType\@Version@.
--
-- 'entityIdentifier', 'describeEntityResponse_entityIdentifier' - The identifier of the entity, in the format of @EntityId\@RevisionId@.
--
-- 'entityArn', 'describeEntityResponse_entityArn' - The ARN associated to the unique identifier for the entity referenced in
-- this request.
--
-- 'httpStatus', 'describeEntityResponse_httpStatus' - The response's http status code.
newDescribeEntityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEntityResponse
newDescribeEntityResponse pHttpStatus_ =
  DescribeEntityResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      details = Prelude.Nothing,
      entityType = Prelude.Nothing,
      entityIdentifier = Prelude.Nothing,
      entityArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The last modified date of the entity, in ISO 8601 format
-- (2018-02-27T13:45:22Z).
describeEntityResponse_lastModifiedDate :: Lens.Lens' DescribeEntityResponse (Prelude.Maybe Prelude.Text)
describeEntityResponse_lastModifiedDate = Lens.lens (\DescribeEntityResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeEntityResponse' {} a -> s {lastModifiedDate = a} :: DescribeEntityResponse)

-- | This stringified JSON object includes the details of the entity.
describeEntityResponse_details :: Lens.Lens' DescribeEntityResponse (Prelude.Maybe Prelude.Text)
describeEntityResponse_details = Lens.lens (\DescribeEntityResponse' {details} -> details) (\s@DescribeEntityResponse' {} a -> s {details = a} :: DescribeEntityResponse)

-- | The named type of the entity, in the format of @EntityType\@Version@.
describeEntityResponse_entityType :: Lens.Lens' DescribeEntityResponse (Prelude.Maybe Prelude.Text)
describeEntityResponse_entityType = Lens.lens (\DescribeEntityResponse' {entityType} -> entityType) (\s@DescribeEntityResponse' {} a -> s {entityType = a} :: DescribeEntityResponse)

-- | The identifier of the entity, in the format of @EntityId\@RevisionId@.
describeEntityResponse_entityIdentifier :: Lens.Lens' DescribeEntityResponse (Prelude.Maybe Prelude.Text)
describeEntityResponse_entityIdentifier = Lens.lens (\DescribeEntityResponse' {entityIdentifier} -> entityIdentifier) (\s@DescribeEntityResponse' {} a -> s {entityIdentifier = a} :: DescribeEntityResponse)

-- | The ARN associated to the unique identifier for the entity referenced in
-- this request.
describeEntityResponse_entityArn :: Lens.Lens' DescribeEntityResponse (Prelude.Maybe Prelude.Text)
describeEntityResponse_entityArn = Lens.lens (\DescribeEntityResponse' {entityArn} -> entityArn) (\s@DescribeEntityResponse' {} a -> s {entityArn = a} :: DescribeEntityResponse)

-- | The response's http status code.
describeEntityResponse_httpStatus :: Lens.Lens' DescribeEntityResponse Prelude.Int
describeEntityResponse_httpStatus = Lens.lens (\DescribeEntityResponse' {httpStatus} -> httpStatus) (\s@DescribeEntityResponse' {} a -> s {httpStatus = a} :: DescribeEntityResponse)

instance Prelude.NFData DescribeEntityResponse where
  rnf DescribeEntityResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf entityType
      `Prelude.seq` Prelude.rnf entityIdentifier
      `Prelude.seq` Prelude.rnf entityArn
      `Prelude.seq` Prelude.rnf httpStatus
