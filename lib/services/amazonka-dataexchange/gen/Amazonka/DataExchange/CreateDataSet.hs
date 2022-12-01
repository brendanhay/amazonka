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
-- Module      : Amazonka.DataExchange.CreateDataSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a data set.
module Amazonka.DataExchange.CreateDataSet
  ( -- * Creating a Request
    CreateDataSet (..),
    newCreateDataSet,

    -- * Request Lenses
    createDataSet_tags,
    createDataSet_assetType,
    createDataSet_description,
    createDataSet_name,

    -- * Destructuring the Response
    CreateDataSetResponse (..),
    newCreateDataSetResponse,

    -- * Response Lenses
    createDataSetResponse_tags,
    createDataSetResponse_name,
    createDataSetResponse_sourceId,
    createDataSetResponse_originDetails,
    createDataSetResponse_arn,
    createDataSetResponse_id,
    createDataSetResponse_assetType,
    createDataSetResponse_description,
    createDataSetResponse_origin,
    createDataSetResponse_createdAt,
    createDataSetResponse_updatedAt,
    createDataSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataSet' smart constructor.
data CreateDataSet = CreateDataSet'
  { -- | A data set tag is an optional label that you can assign to a data set
    -- when you create it. Each tag consists of a key and an optional value,
    -- both of which you define. When you use tagging, you can also use
    -- tag-based access control in IAM policies to control access to these data
    -- sets and revisions.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of asset that is added to a data set.
    assetType :: AssetType,
    -- | A description for the data set. This value can be up to 16,348
    -- characters long.
    description :: Prelude.Text,
    -- | The name of the data set.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDataSet_tags' - A data set tag is an optional label that you can assign to a data set
-- when you create it. Each tag consists of a key and an optional value,
-- both of which you define. When you use tagging, you can also use
-- tag-based access control in IAM policies to control access to these data
-- sets and revisions.
--
-- 'assetType', 'createDataSet_assetType' - The type of asset that is added to a data set.
--
-- 'description', 'createDataSet_description' - A description for the data set. This value can be up to 16,348
-- characters long.
--
-- 'name', 'createDataSet_name' - The name of the data set.
newCreateDataSet ::
  -- | 'assetType'
  AssetType ->
  -- | 'description'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateDataSet
newCreateDataSet pAssetType_ pDescription_ pName_ =
  CreateDataSet'
    { tags = Prelude.Nothing,
      assetType = pAssetType_,
      description = pDescription_,
      name = pName_
    }

-- | A data set tag is an optional label that you can assign to a data set
-- when you create it. Each tag consists of a key and an optional value,
-- both of which you define. When you use tagging, you can also use
-- tag-based access control in IAM policies to control access to these data
-- sets and revisions.
createDataSet_tags :: Lens.Lens' CreateDataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataSet_tags = Lens.lens (\CreateDataSet' {tags} -> tags) (\s@CreateDataSet' {} a -> s {tags = a} :: CreateDataSet) Prelude.. Lens.mapping Lens.coerced

-- | The type of asset that is added to a data set.
createDataSet_assetType :: Lens.Lens' CreateDataSet AssetType
createDataSet_assetType = Lens.lens (\CreateDataSet' {assetType} -> assetType) (\s@CreateDataSet' {} a -> s {assetType = a} :: CreateDataSet)

-- | A description for the data set. This value can be up to 16,348
-- characters long.
createDataSet_description :: Lens.Lens' CreateDataSet Prelude.Text
createDataSet_description = Lens.lens (\CreateDataSet' {description} -> description) (\s@CreateDataSet' {} a -> s {description = a} :: CreateDataSet)

-- | The name of the data set.
createDataSet_name :: Lens.Lens' CreateDataSet Prelude.Text
createDataSet_name = Lens.lens (\CreateDataSet' {name} -> name) (\s@CreateDataSet' {} a -> s {name = a} :: CreateDataSet)

instance Core.AWSRequest CreateDataSet where
  type
    AWSResponse CreateDataSet =
      CreateDataSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSetResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "SourceId")
            Prelude.<*> (x Core..?> "OriginDetails")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "AssetType")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "Origin")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSet where
  hashWithSalt _salt CreateDataSet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assetType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateDataSet where
  rnf CreateDataSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateDataSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDataSet where
  toJSON CreateDataSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("AssetType" Core..= assetType),
            Prelude.Just ("Description" Core..= description),
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateDataSet where
  toPath = Prelude.const "/v1/data-sets"

instance Core.ToQuery CreateDataSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataSetResponse' smart constructor.
data CreateDataSetResponse = CreateDataSetResponse'
  { -- | The tags for the data set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the data set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The data set ID of the owned data set corresponding to the entitled data
    -- set being viewed. This parameter is returned when a data set owner is
    -- viewing the entitled copy of its owned data set.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | If the origin of this data set is ENTITLED, includes the details for the
    -- product on AWS Marketplace.
    originDetails :: Prelude.Maybe OriginDetails,
    -- | The ARN for the data set.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the data set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of asset that is added to a data set.
    assetType :: Prelude.Maybe AssetType,
    -- | The description for the data set.
    description :: Prelude.Maybe Prelude.Text,
    -- | A property that defines the data set as OWNED by the account (for
    -- providers) or ENTITLED to the account (for subscribers).
    origin :: Prelude.Maybe Origin,
    -- | The date and time that the data set was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the data set was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDataSetResponse_tags' - The tags for the data set.
--
-- 'name', 'createDataSetResponse_name' - The name of the data set.
--
-- 'sourceId', 'createDataSetResponse_sourceId' - The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
--
-- 'originDetails', 'createDataSetResponse_originDetails' - If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
--
-- 'arn', 'createDataSetResponse_arn' - The ARN for the data set.
--
-- 'id', 'createDataSetResponse_id' - The unique identifier for the data set.
--
-- 'assetType', 'createDataSetResponse_assetType' - The type of asset that is added to a data set.
--
-- 'description', 'createDataSetResponse_description' - The description for the data set.
--
-- 'origin', 'createDataSetResponse_origin' - A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
--
-- 'createdAt', 'createDataSetResponse_createdAt' - The date and time that the data set was created, in ISO 8601 format.
--
-- 'updatedAt', 'createDataSetResponse_updatedAt' - The date and time that the data set was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'createDataSetResponse_httpStatus' - The response's http status code.
newCreateDataSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataSetResponse
newCreateDataSetResponse pHttpStatus_ =
  CreateDataSetResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      originDetails = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      assetType = Prelude.Nothing,
      description = Prelude.Nothing,
      origin = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags for the data set.
createDataSetResponse_tags :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataSetResponse_tags = Lens.lens (\CreateDataSetResponse' {tags} -> tags) (\s@CreateDataSetResponse' {} a -> s {tags = a} :: CreateDataSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data set.
createDataSetResponse_name :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.Text)
createDataSetResponse_name = Lens.lens (\CreateDataSetResponse' {name} -> name) (\s@CreateDataSetResponse' {} a -> s {name = a} :: CreateDataSetResponse)

-- | The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
createDataSetResponse_sourceId :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.Text)
createDataSetResponse_sourceId = Lens.lens (\CreateDataSetResponse' {sourceId} -> sourceId) (\s@CreateDataSetResponse' {} a -> s {sourceId = a} :: CreateDataSetResponse)

-- | If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
createDataSetResponse_originDetails :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe OriginDetails)
createDataSetResponse_originDetails = Lens.lens (\CreateDataSetResponse' {originDetails} -> originDetails) (\s@CreateDataSetResponse' {} a -> s {originDetails = a} :: CreateDataSetResponse)

-- | The ARN for the data set.
createDataSetResponse_arn :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.Text)
createDataSetResponse_arn = Lens.lens (\CreateDataSetResponse' {arn} -> arn) (\s@CreateDataSetResponse' {} a -> s {arn = a} :: CreateDataSetResponse)

-- | The unique identifier for the data set.
createDataSetResponse_id :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.Text)
createDataSetResponse_id = Lens.lens (\CreateDataSetResponse' {id} -> id) (\s@CreateDataSetResponse' {} a -> s {id = a} :: CreateDataSetResponse)

-- | The type of asset that is added to a data set.
createDataSetResponse_assetType :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe AssetType)
createDataSetResponse_assetType = Lens.lens (\CreateDataSetResponse' {assetType} -> assetType) (\s@CreateDataSetResponse' {} a -> s {assetType = a} :: CreateDataSetResponse)

-- | The description for the data set.
createDataSetResponse_description :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.Text)
createDataSetResponse_description = Lens.lens (\CreateDataSetResponse' {description} -> description) (\s@CreateDataSetResponse' {} a -> s {description = a} :: CreateDataSetResponse)

-- | A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
createDataSetResponse_origin :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Origin)
createDataSetResponse_origin = Lens.lens (\CreateDataSetResponse' {origin} -> origin) (\s@CreateDataSetResponse' {} a -> s {origin = a} :: CreateDataSetResponse)

-- | The date and time that the data set was created, in ISO 8601 format.
createDataSetResponse_createdAt :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.UTCTime)
createDataSetResponse_createdAt = Lens.lens (\CreateDataSetResponse' {createdAt} -> createdAt) (\s@CreateDataSetResponse' {} a -> s {createdAt = a} :: CreateDataSetResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time that the data set was last updated, in ISO 8601
-- format.
createDataSetResponse_updatedAt :: Lens.Lens' CreateDataSetResponse (Prelude.Maybe Prelude.UTCTime)
createDataSetResponse_updatedAt = Lens.lens (\CreateDataSetResponse' {updatedAt} -> updatedAt) (\s@CreateDataSetResponse' {} a -> s {updatedAt = a} :: CreateDataSetResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
createDataSetResponse_httpStatus :: Lens.Lens' CreateDataSetResponse Prelude.Int
createDataSetResponse_httpStatus = Lens.lens (\CreateDataSetResponse' {httpStatus} -> httpStatus) (\s@CreateDataSetResponse' {} a -> s {httpStatus = a} :: CreateDataSetResponse)

instance Prelude.NFData CreateDataSetResponse where
  rnf CreateDataSetResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf originDetails
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf origin
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
