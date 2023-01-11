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
-- Module      : Amazonka.IoT.CreateThingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new thing type.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateThingType>
-- action.
module Amazonka.IoT.CreateThingType
  ( -- * Creating a Request
    CreateThingType (..),
    newCreateThingType,

    -- * Request Lenses
    createThingType_tags,
    createThingType_thingTypeProperties,
    createThingType_thingTypeName,

    -- * Destructuring the Response
    CreateThingTypeResponse (..),
    newCreateThingTypeResponse,

    -- * Response Lenses
    createThingTypeResponse_thingTypeArn,
    createThingTypeResponse_thingTypeId,
    createThingTypeResponse_thingTypeName,
    createThingTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CreateThingType operation.
--
-- /See:/ 'newCreateThingType' smart constructor.
data CreateThingType = CreateThingType'
  { -- | Metadata which can be used to manage the thing type.
    tags :: Prelude.Maybe [Tag],
    -- | The ThingTypeProperties for the thing type to create. It contains
    -- information about the new thing type including a description, and a list
    -- of searchable thing attribute names.
    thingTypeProperties :: Prelude.Maybe ThingTypeProperties,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThingType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createThingType_tags' - Metadata which can be used to manage the thing type.
--
-- 'thingTypeProperties', 'createThingType_thingTypeProperties' - The ThingTypeProperties for the thing type to create. It contains
-- information about the new thing type including a description, and a list
-- of searchable thing attribute names.
--
-- 'thingTypeName', 'createThingType_thingTypeName' - The name of the thing type.
newCreateThingType ::
  -- | 'thingTypeName'
  Prelude.Text ->
  CreateThingType
newCreateThingType pThingTypeName_ =
  CreateThingType'
    { tags = Prelude.Nothing,
      thingTypeProperties = Prelude.Nothing,
      thingTypeName = pThingTypeName_
    }

-- | Metadata which can be used to manage the thing type.
createThingType_tags :: Lens.Lens' CreateThingType (Prelude.Maybe [Tag])
createThingType_tags = Lens.lens (\CreateThingType' {tags} -> tags) (\s@CreateThingType' {} a -> s {tags = a} :: CreateThingType) Prelude.. Lens.mapping Lens.coerced

-- | The ThingTypeProperties for the thing type to create. It contains
-- information about the new thing type including a description, and a list
-- of searchable thing attribute names.
createThingType_thingTypeProperties :: Lens.Lens' CreateThingType (Prelude.Maybe ThingTypeProperties)
createThingType_thingTypeProperties = Lens.lens (\CreateThingType' {thingTypeProperties} -> thingTypeProperties) (\s@CreateThingType' {} a -> s {thingTypeProperties = a} :: CreateThingType)

-- | The name of the thing type.
createThingType_thingTypeName :: Lens.Lens' CreateThingType Prelude.Text
createThingType_thingTypeName = Lens.lens (\CreateThingType' {thingTypeName} -> thingTypeName) (\s@CreateThingType' {} a -> s {thingTypeName = a} :: CreateThingType)

instance Core.AWSRequest CreateThingType where
  type
    AWSResponse CreateThingType =
      CreateThingTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingTypeResponse'
            Prelude.<$> (x Data..?> "thingTypeArn")
            Prelude.<*> (x Data..?> "thingTypeId")
            Prelude.<*> (x Data..?> "thingTypeName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThingType where
  hashWithSalt _salt CreateThingType' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` thingTypeProperties
      `Prelude.hashWithSalt` thingTypeName

instance Prelude.NFData CreateThingType where
  rnf CreateThingType' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf thingTypeProperties
      `Prelude.seq` Prelude.rnf thingTypeName

instance Data.ToHeaders CreateThingType where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateThingType where
  toJSON CreateThingType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("thingTypeProperties" Data..=)
              Prelude.<$> thingTypeProperties
          ]
      )

instance Data.ToPath CreateThingType where
  toPath CreateThingType' {..} =
    Prelude.mconcat
      ["/thing-types/", Data.toBS thingTypeName]

instance Data.ToQuery CreateThingType where
  toQuery = Prelude.const Prelude.mempty

-- | The output of the CreateThingType operation.
--
-- /See:/ 'newCreateThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the thing type.
    thingTypeArn :: Prelude.Maybe Prelude.Text,
    -- | The thing type ID.
    thingTypeId :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThingTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeArn', 'createThingTypeResponse_thingTypeArn' - The Amazon Resource Name (ARN) of the thing type.
--
-- 'thingTypeId', 'createThingTypeResponse_thingTypeId' - The thing type ID.
--
-- 'thingTypeName', 'createThingTypeResponse_thingTypeName' - The name of the thing type.
--
-- 'httpStatus', 'createThingTypeResponse_httpStatus' - The response's http status code.
newCreateThingTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateThingTypeResponse
newCreateThingTypeResponse pHttpStatus_ =
  CreateThingTypeResponse'
    { thingTypeArn =
        Prelude.Nothing,
      thingTypeId = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the thing type.
createThingTypeResponse_thingTypeArn :: Lens.Lens' CreateThingTypeResponse (Prelude.Maybe Prelude.Text)
createThingTypeResponse_thingTypeArn = Lens.lens (\CreateThingTypeResponse' {thingTypeArn} -> thingTypeArn) (\s@CreateThingTypeResponse' {} a -> s {thingTypeArn = a} :: CreateThingTypeResponse)

-- | The thing type ID.
createThingTypeResponse_thingTypeId :: Lens.Lens' CreateThingTypeResponse (Prelude.Maybe Prelude.Text)
createThingTypeResponse_thingTypeId = Lens.lens (\CreateThingTypeResponse' {thingTypeId} -> thingTypeId) (\s@CreateThingTypeResponse' {} a -> s {thingTypeId = a} :: CreateThingTypeResponse)

-- | The name of the thing type.
createThingTypeResponse_thingTypeName :: Lens.Lens' CreateThingTypeResponse (Prelude.Maybe Prelude.Text)
createThingTypeResponse_thingTypeName = Lens.lens (\CreateThingTypeResponse' {thingTypeName} -> thingTypeName) (\s@CreateThingTypeResponse' {} a -> s {thingTypeName = a} :: CreateThingTypeResponse)

-- | The response's http status code.
createThingTypeResponse_httpStatus :: Lens.Lens' CreateThingTypeResponse Prelude.Int
createThingTypeResponse_httpStatus = Lens.lens (\CreateThingTypeResponse' {httpStatus} -> httpStatus) (\s@CreateThingTypeResponse' {} a -> s {httpStatus = a} :: CreateThingTypeResponse)

instance Prelude.NFData CreateThingTypeResponse where
  rnf CreateThingTypeResponse' {..} =
    Prelude.rnf thingTypeArn
      `Prelude.seq` Prelude.rnf thingTypeId
      `Prelude.seq` Prelude.rnf thingTypeName
      `Prelude.seq` Prelude.rnf httpStatus
