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
-- Module      : Amazonka.ServiceCatalogAppRegistry.GetAttributeGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an attribute group, either by its name or its ID. The
-- attribute group can be specified either by its unique ID or by its name.
module Amazonka.ServiceCatalogAppRegistry.GetAttributeGroup
  ( -- * Creating a Request
    GetAttributeGroup (..),
    newGetAttributeGroup,

    -- * Request Lenses
    getAttributeGroup_attributeGroup,

    -- * Destructuring the Response
    GetAttributeGroupResponse (..),
    newGetAttributeGroupResponse,

    -- * Response Lenses
    getAttributeGroupResponse_tags,
    getAttributeGroupResponse_name,
    getAttributeGroupResponse_arn,
    getAttributeGroupResponse_description,
    getAttributeGroupResponse_id,
    getAttributeGroupResponse_creationTime,
    getAttributeGroupResponse_lastUpdateTime,
    getAttributeGroupResponse_attributes,
    getAttributeGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newGetAttributeGroup' smart constructor.
data GetAttributeGroup = GetAttributeGroup'
  { -- | The name or ID of the attribute group that holds the attributes to
    -- describe the application.
    attributeGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttributeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeGroup', 'getAttributeGroup_attributeGroup' - The name or ID of the attribute group that holds the attributes to
-- describe the application.
newGetAttributeGroup ::
  -- | 'attributeGroup'
  Prelude.Text ->
  GetAttributeGroup
newGetAttributeGroup pAttributeGroup_ =
  GetAttributeGroup'
    { attributeGroup =
        pAttributeGroup_
    }

-- | The name or ID of the attribute group that holds the attributes to
-- describe the application.
getAttributeGroup_attributeGroup :: Lens.Lens' GetAttributeGroup Prelude.Text
getAttributeGroup_attributeGroup = Lens.lens (\GetAttributeGroup' {attributeGroup} -> attributeGroup) (\s@GetAttributeGroup' {} a -> s {attributeGroup = a} :: GetAttributeGroup)

instance Core.AWSRequest GetAttributeGroup where
  type
    AWSResponse GetAttributeGroup =
      GetAttributeGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAttributeGroupResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "lastUpdateTime")
            Prelude.<*> (x Data..?> "attributes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAttributeGroup where
  hashWithSalt _salt GetAttributeGroup' {..} =
    _salt `Prelude.hashWithSalt` attributeGroup

instance Prelude.NFData GetAttributeGroup where
  rnf GetAttributeGroup' {..} =
    Prelude.rnf attributeGroup

instance Data.ToHeaders GetAttributeGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAttributeGroup where
  toPath GetAttributeGroup' {..} =
    Prelude.mconcat
      ["/attribute-groups/", Data.toBS attributeGroup]

instance Data.ToQuery GetAttributeGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAttributeGroupResponse' smart constructor.
data GetAttributeGroupResponse = GetAttributeGroupResponse'
  { -- | Key-value pairs associated with the attribute group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the attribute group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon resource name (ARN) that specifies the attribute group across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the attribute group that the user provides.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the attribute group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment the attribute group was
    -- created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ISO-8601 formatted timestamp of the moment the attribute group was
    -- last updated. This time is the same as the creationTime for a newly
    -- created attribute group.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | A JSON string in the form of nested key-value pairs that represent the
    -- attributes in the group and describes an application and its components.
    attributes :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttributeGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getAttributeGroupResponse_tags' - Key-value pairs associated with the attribute group.
--
-- 'name', 'getAttributeGroupResponse_name' - The name of the attribute group.
--
-- 'arn', 'getAttributeGroupResponse_arn' - The Amazon resource name (ARN) that specifies the attribute group across
-- services.
--
-- 'description', 'getAttributeGroupResponse_description' - The description of the attribute group that the user provides.
--
-- 'id', 'getAttributeGroupResponse_id' - The identifier of the attribute group.
--
-- 'creationTime', 'getAttributeGroupResponse_creationTime' - The ISO-8601 formatted timestamp of the moment the attribute group was
-- created.
--
-- 'lastUpdateTime', 'getAttributeGroupResponse_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment the attribute group was
-- last updated. This time is the same as the creationTime for a newly
-- created attribute group.
--
-- 'attributes', 'getAttributeGroupResponse_attributes' - A JSON string in the form of nested key-value pairs that represent the
-- attributes in the group and describes an application and its components.
--
-- 'httpStatus', 'getAttributeGroupResponse_httpStatus' - The response's http status code.
newGetAttributeGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAttributeGroupResponse
newGetAttributeGroupResponse pHttpStatus_ =
  GetAttributeGroupResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      attributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Key-value pairs associated with the attribute group.
getAttributeGroupResponse_tags :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getAttributeGroupResponse_tags = Lens.lens (\GetAttributeGroupResponse' {tags} -> tags) (\s@GetAttributeGroupResponse' {} a -> s {tags = a} :: GetAttributeGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the attribute group.
getAttributeGroupResponse_name :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.Text)
getAttributeGroupResponse_name = Lens.lens (\GetAttributeGroupResponse' {name} -> name) (\s@GetAttributeGroupResponse' {} a -> s {name = a} :: GetAttributeGroupResponse)

-- | The Amazon resource name (ARN) that specifies the attribute group across
-- services.
getAttributeGroupResponse_arn :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.Text)
getAttributeGroupResponse_arn = Lens.lens (\GetAttributeGroupResponse' {arn} -> arn) (\s@GetAttributeGroupResponse' {} a -> s {arn = a} :: GetAttributeGroupResponse)

-- | The description of the attribute group that the user provides.
getAttributeGroupResponse_description :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.Text)
getAttributeGroupResponse_description = Lens.lens (\GetAttributeGroupResponse' {description} -> description) (\s@GetAttributeGroupResponse' {} a -> s {description = a} :: GetAttributeGroupResponse)

-- | The identifier of the attribute group.
getAttributeGroupResponse_id :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.Text)
getAttributeGroupResponse_id = Lens.lens (\GetAttributeGroupResponse' {id} -> id) (\s@GetAttributeGroupResponse' {} a -> s {id = a} :: GetAttributeGroupResponse)

-- | The ISO-8601 formatted timestamp of the moment the attribute group was
-- created.
getAttributeGroupResponse_creationTime :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.UTCTime)
getAttributeGroupResponse_creationTime = Lens.lens (\GetAttributeGroupResponse' {creationTime} -> creationTime) (\s@GetAttributeGroupResponse' {} a -> s {creationTime = a} :: GetAttributeGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The ISO-8601 formatted timestamp of the moment the attribute group was
-- last updated. This time is the same as the creationTime for a newly
-- created attribute group.
getAttributeGroupResponse_lastUpdateTime :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.UTCTime)
getAttributeGroupResponse_lastUpdateTime = Lens.lens (\GetAttributeGroupResponse' {lastUpdateTime} -> lastUpdateTime) (\s@GetAttributeGroupResponse' {} a -> s {lastUpdateTime = a} :: GetAttributeGroupResponse) Prelude.. Lens.mapping Data._Time

-- | A JSON string in the form of nested key-value pairs that represent the
-- attributes in the group and describes an application and its components.
getAttributeGroupResponse_attributes :: Lens.Lens' GetAttributeGroupResponse (Prelude.Maybe Prelude.Text)
getAttributeGroupResponse_attributes = Lens.lens (\GetAttributeGroupResponse' {attributes} -> attributes) (\s@GetAttributeGroupResponse' {} a -> s {attributes = a} :: GetAttributeGroupResponse)

-- | The response's http status code.
getAttributeGroupResponse_httpStatus :: Lens.Lens' GetAttributeGroupResponse Prelude.Int
getAttributeGroupResponse_httpStatus = Lens.lens (\GetAttributeGroupResponse' {httpStatus} -> httpStatus) (\s@GetAttributeGroupResponse' {} a -> s {httpStatus = a} :: GetAttributeGroupResponse)

instance Prelude.NFData GetAttributeGroupResponse where
  rnf GetAttributeGroupResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
