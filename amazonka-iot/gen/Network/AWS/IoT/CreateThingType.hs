{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateThingType operation.
--
-- /See:/ 'newCreateThingType' smart constructor.
data CreateThingType = CreateThingType'
  { -- | The ThingTypeProperties for the thing type to create. It contains
    -- information about the new thing type including a description, and a list
    -- of searchable thing attribute names.
    thingTypeProperties :: Prelude.Maybe ThingTypeProperties,
    -- | Metadata which can be used to manage the thing type.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the thing type.
    thingTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateThingType
newCreateThingType pThingTypeName_ =
  CreateThingType'
    { thingTypeProperties =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      thingTypeName = pThingTypeName_
    }

-- | The ThingTypeProperties for the thing type to create. It contains
-- information about the new thing type including a description, and a list
-- of searchable thing attribute names.
createThingType_thingTypeProperties :: Lens.Lens' CreateThingType (Prelude.Maybe ThingTypeProperties)
createThingType_thingTypeProperties = Lens.lens (\CreateThingType' {thingTypeProperties} -> thingTypeProperties) (\s@CreateThingType' {} a -> s {thingTypeProperties = a} :: CreateThingType)

-- | Metadata which can be used to manage the thing type.
createThingType_tags :: Lens.Lens' CreateThingType (Prelude.Maybe [Tag])
createThingType_tags = Lens.lens (\CreateThingType' {tags} -> tags) (\s@CreateThingType' {} a -> s {tags = a} :: CreateThingType) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the thing type.
createThingType_thingTypeName :: Lens.Lens' CreateThingType Prelude.Text
createThingType_thingTypeName = Lens.lens (\CreateThingType' {thingTypeName} -> thingTypeName) (\s@CreateThingType' {} a -> s {thingTypeName = a} :: CreateThingType)

instance Prelude.AWSRequest CreateThingType where
  type Rs CreateThingType = CreateThingTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingTypeResponse'
            Prelude.<$> (x Prelude..?> "thingTypeId")
            Prelude.<*> (x Prelude..?> "thingTypeArn")
            Prelude.<*> (x Prelude..?> "thingTypeName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThingType

instance Prelude.NFData CreateThingType

instance Prelude.ToHeaders CreateThingType where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateThingType where
  toJSON CreateThingType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("thingTypeProperties" Prelude..=)
              Prelude.<$> thingTypeProperties,
            ("tags" Prelude..=) Prelude.<$> tags
          ]
      )

instance Prelude.ToPath CreateThingType where
  toPath CreateThingType' {..} =
    Prelude.mconcat
      ["/thing-types/", Prelude.toBS thingTypeName]

instance Prelude.ToQuery CreateThingType where
  toQuery = Prelude.const Prelude.mempty

-- | The output of the CreateThingType operation.
--
-- /See:/ 'newCreateThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { -- | The thing type ID.
    thingTypeId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the thing type.
    thingTypeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateThingTypeResponse
newCreateThingTypeResponse pHttpStatus_ =
  CreateThingTypeResponse'
    { thingTypeId =
        Prelude.Nothing,
      thingTypeArn = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing type ID.
createThingTypeResponse_thingTypeId :: Lens.Lens' CreateThingTypeResponse (Prelude.Maybe Prelude.Text)
createThingTypeResponse_thingTypeId = Lens.lens (\CreateThingTypeResponse' {thingTypeId} -> thingTypeId) (\s@CreateThingTypeResponse' {} a -> s {thingTypeId = a} :: CreateThingTypeResponse)

-- | The Amazon Resource Name (ARN) of the thing type.
createThingTypeResponse_thingTypeArn :: Lens.Lens' CreateThingTypeResponse (Prelude.Maybe Prelude.Text)
createThingTypeResponse_thingTypeArn = Lens.lens (\CreateThingTypeResponse' {thingTypeArn} -> thingTypeArn) (\s@CreateThingTypeResponse' {} a -> s {thingTypeArn = a} :: CreateThingTypeResponse)

-- | The name of the thing type.
createThingTypeResponse_thingTypeName :: Lens.Lens' CreateThingTypeResponse (Prelude.Maybe Prelude.Text)
createThingTypeResponse_thingTypeName = Lens.lens (\CreateThingTypeResponse' {thingTypeName} -> thingTypeName) (\s@CreateThingTypeResponse' {} a -> s {thingTypeName = a} :: CreateThingTypeResponse)

-- | The response's http status code.
createThingTypeResponse_httpStatus :: Lens.Lens' CreateThingTypeResponse Prelude.Int
createThingTypeResponse_httpStatus = Lens.lens (\CreateThingTypeResponse' {httpStatus} -> httpStatus) (\s@CreateThingTypeResponse' {} a -> s {httpStatus = a} :: CreateThingTypeResponse)

instance Prelude.NFData CreateThingTypeResponse
