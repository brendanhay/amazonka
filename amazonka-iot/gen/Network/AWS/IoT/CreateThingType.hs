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
-- Module      : Network.AWS.IoT.CreateThingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new thing type.
module Network.AWS.IoT.CreateThingType
  ( -- * Creating a Request
    CreateThingType (..),
    newCreateThingType,

    -- * Request Lenses
    createThingType_thingTypeProperties,
    createThingType_tags,
    createThingType_thingTypeName,

    -- * Destructuring the Response
    CreateThingTypeResponse (..),
    newCreateThingTypeResponse,

    -- * Response Lenses
    createThingTypeResponse_thingTypeId,
    createThingTypeResponse_thingTypeArn,
    createThingTypeResponse_thingTypeName,
    createThingTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateThingType operation.
--
-- /See:/ 'newCreateThingType' smart constructor.
data CreateThingType = CreateThingType'
  { -- | The ThingTypeProperties for the thing type to create. It contains
    -- information about the new thing type including a description, and a list
    -- of searchable thing attribute names.
    thingTypeProperties :: Core.Maybe ThingTypeProperties,
    -- | Metadata which can be used to manage the thing type.
    tags :: Core.Maybe [Tag],
    -- | The name of the thing type.
    thingTypeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateThingType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeProperties', 'createThingType_thingTypeProperties' - The ThingTypeProperties for the thing type to create. It contains
-- information about the new thing type including a description, and a list
-- of searchable thing attribute names.
--
-- 'tags', 'createThingType_tags' - Metadata which can be used to manage the thing type.
--
-- 'thingTypeName', 'createThingType_thingTypeName' - The name of the thing type.
newCreateThingType ::
  -- | 'thingTypeName'
  Core.Text ->
  CreateThingType
newCreateThingType pThingTypeName_ =
  CreateThingType'
    { thingTypeProperties =
        Core.Nothing,
      tags = Core.Nothing,
      thingTypeName = pThingTypeName_
    }

-- | The ThingTypeProperties for the thing type to create. It contains
-- information about the new thing type including a description, and a list
-- of searchable thing attribute names.
createThingType_thingTypeProperties :: Lens.Lens' CreateThingType (Core.Maybe ThingTypeProperties)
createThingType_thingTypeProperties = Lens.lens (\CreateThingType' {thingTypeProperties} -> thingTypeProperties) (\s@CreateThingType' {} a -> s {thingTypeProperties = a} :: CreateThingType)

-- | Metadata which can be used to manage the thing type.
createThingType_tags :: Lens.Lens' CreateThingType (Core.Maybe [Tag])
createThingType_tags = Lens.lens (\CreateThingType' {tags} -> tags) (\s@CreateThingType' {} a -> s {tags = a} :: CreateThingType) Core.. Lens.mapping Lens._Coerce

-- | The name of the thing type.
createThingType_thingTypeName :: Lens.Lens' CreateThingType Core.Text
createThingType_thingTypeName = Lens.lens (\CreateThingType' {thingTypeName} -> thingTypeName) (\s@CreateThingType' {} a -> s {thingTypeName = a} :: CreateThingType)

instance Core.AWSRequest CreateThingType where
  type
    AWSResponse CreateThingType =
      CreateThingTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingTypeResponse'
            Core.<$> (x Core..?> "thingTypeId")
            Core.<*> (x Core..?> "thingTypeArn")
            Core.<*> (x Core..?> "thingTypeName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateThingType

instance Core.NFData CreateThingType

instance Core.ToHeaders CreateThingType where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateThingType where
  toJSON CreateThingType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("thingTypeProperties" Core..=)
              Core.<$> thingTypeProperties,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.ToPath CreateThingType where
  toPath CreateThingType' {..} =
    Core.mconcat
      ["/thing-types/", Core.toBS thingTypeName]

instance Core.ToQuery CreateThingType where
  toQuery = Core.const Core.mempty

-- | The output of the CreateThingType operation.
--
-- /See:/ 'newCreateThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { -- | The thing type ID.
    thingTypeId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the thing type.
    thingTypeArn :: Core.Maybe Core.Text,
    -- | The name of the thing type.
    thingTypeName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateThingTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeId', 'createThingTypeResponse_thingTypeId' - The thing type ID.
--
-- 'thingTypeArn', 'createThingTypeResponse_thingTypeArn' - The Amazon Resource Name (ARN) of the thing type.
--
-- 'thingTypeName', 'createThingTypeResponse_thingTypeName' - The name of the thing type.
--
-- 'httpStatus', 'createThingTypeResponse_httpStatus' - The response's http status code.
newCreateThingTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateThingTypeResponse
newCreateThingTypeResponse pHttpStatus_ =
  CreateThingTypeResponse'
    { thingTypeId =
        Core.Nothing,
      thingTypeArn = Core.Nothing,
      thingTypeName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing type ID.
createThingTypeResponse_thingTypeId :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Core.Text)
createThingTypeResponse_thingTypeId = Lens.lens (\CreateThingTypeResponse' {thingTypeId} -> thingTypeId) (\s@CreateThingTypeResponse' {} a -> s {thingTypeId = a} :: CreateThingTypeResponse)

-- | The Amazon Resource Name (ARN) of the thing type.
createThingTypeResponse_thingTypeArn :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Core.Text)
createThingTypeResponse_thingTypeArn = Lens.lens (\CreateThingTypeResponse' {thingTypeArn} -> thingTypeArn) (\s@CreateThingTypeResponse' {} a -> s {thingTypeArn = a} :: CreateThingTypeResponse)

-- | The name of the thing type.
createThingTypeResponse_thingTypeName :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Core.Text)
createThingTypeResponse_thingTypeName = Lens.lens (\CreateThingTypeResponse' {thingTypeName} -> thingTypeName) (\s@CreateThingTypeResponse' {} a -> s {thingTypeName = a} :: CreateThingTypeResponse)

-- | The response's http status code.
createThingTypeResponse_httpStatus :: Lens.Lens' CreateThingTypeResponse Core.Int
createThingTypeResponse_httpStatus = Lens.lens (\CreateThingTypeResponse' {httpStatus} -> httpStatus) (\s@CreateThingTypeResponse' {} a -> s {httpStatus = a} :: CreateThingTypeResponse)

instance Core.NFData CreateThingTypeResponse
