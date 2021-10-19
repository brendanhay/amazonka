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
-- Module      : Network.AWS.LexModels.GetSlotType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific version of a slot type. In addition
-- to specifying the slot type name, you must specify the slot type
-- version.
--
-- This operation requires permissions for the @lex:GetSlotType@ action.
module Network.AWS.LexModels.GetSlotType
  ( -- * Creating a Request
    GetSlotType (..),
    newGetSlotType,

    -- * Request Lenses
    getSlotType_name,
    getSlotType_version,

    -- * Destructuring the Response
    GetSlotTypeResponse (..),
    newGetSlotTypeResponse,

    -- * Response Lenses
    getSlotTypeResponse_parentSlotTypeSignature,
    getSlotTypeResponse_slotTypeConfigurations,
    getSlotTypeResponse_checksum,
    getSlotTypeResponse_valueSelectionStrategy,
    getSlotTypeResponse_createdDate,
    getSlotTypeResponse_name,
    getSlotTypeResponse_version,
    getSlotTypeResponse_lastUpdatedDate,
    getSlotTypeResponse_description,
    getSlotTypeResponse_enumerationValues,
    getSlotTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSlotType' smart constructor.
data GetSlotType = GetSlotType'
  { -- | The name of the slot type. The name is case sensitive.
    name :: Prelude.Text,
    -- | The version of the slot type.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getSlotType_name' - The name of the slot type. The name is case sensitive.
--
-- 'version', 'getSlotType_version' - The version of the slot type.
newGetSlotType ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  GetSlotType
newGetSlotType pName_ pVersion_ =
  GetSlotType' {name = pName_, version = pVersion_}

-- | The name of the slot type. The name is case sensitive.
getSlotType_name :: Lens.Lens' GetSlotType Prelude.Text
getSlotType_name = Lens.lens (\GetSlotType' {name} -> name) (\s@GetSlotType' {} a -> s {name = a} :: GetSlotType)

-- | The version of the slot type.
getSlotType_version :: Lens.Lens' GetSlotType Prelude.Text
getSlotType_version = Lens.lens (\GetSlotType' {version} -> version) (\s@GetSlotType' {} a -> s {version = a} :: GetSlotType)

instance Core.AWSRequest GetSlotType where
  type AWSResponse GetSlotType = GetSlotTypeResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypeResponse'
            Prelude.<$> (x Core..?> "parentSlotTypeSignature")
            Prelude.<*> ( x Core..?> "slotTypeConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "checksum")
            Prelude.<*> (x Core..?> "valueSelectionStrategy")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> ( x Core..?> "enumerationValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSlotType

instance Prelude.NFData GetSlotType

instance Core.ToHeaders GetSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSlotType where
  toPath GetSlotType' {..} =
    Prelude.mconcat
      [ "/slottypes/",
        Core.toBS name,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery GetSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSlotTypeResponse' smart constructor.
data GetSlotTypeResponse = GetSlotTypeResponse'
  { -- | The built-in slot type used as a parent for the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Prelude.Maybe [SlotTypeConfiguration],
    -- | Checksum of the @$LATEST@ version of the slot type.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The strategy that Amazon Lex uses to determine the value of the slot.
    -- For more information, see PutSlotType.
    valueSelectionStrategy :: Prelude.Maybe SlotValueSelectionStrategy,
    -- | The date that the slot type was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the slot type.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the slot type.
    version :: Prelude.Maybe Prelude.Text,
    -- | The date that the slot type was updated. When you create a resource, the
    -- creation date and last update date are the same.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the
    -- slot type can take.
    enumerationValues :: Prelude.Maybe [EnumerationValue],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'getSlotTypeResponse_parentSlotTypeSignature' - The built-in slot type used as a parent for the slot type.
--
-- 'slotTypeConfigurations', 'getSlotTypeResponse_slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
--
-- 'checksum', 'getSlotTypeResponse_checksum' - Checksum of the @$LATEST@ version of the slot type.
--
-- 'valueSelectionStrategy', 'getSlotTypeResponse_valueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot.
-- For more information, see PutSlotType.
--
-- 'createdDate', 'getSlotTypeResponse_createdDate' - The date that the slot type was created.
--
-- 'name', 'getSlotTypeResponse_name' - The name of the slot type.
--
-- 'version', 'getSlotTypeResponse_version' - The version of the slot type.
--
-- 'lastUpdatedDate', 'getSlotTypeResponse_lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the
-- creation date and last update date are the same.
--
-- 'description', 'getSlotTypeResponse_description' - A description of the slot type.
--
-- 'enumerationValues', 'getSlotTypeResponse_enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
--
-- 'httpStatus', 'getSlotTypeResponse_httpStatus' - The response's http status code.
newGetSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSlotTypeResponse
newGetSlotTypeResponse pHttpStatus_ =
  GetSlotTypeResponse'
    { parentSlotTypeSignature =
        Prelude.Nothing,
      slotTypeConfigurations = Prelude.Nothing,
      checksum = Prelude.Nothing,
      valueSelectionStrategy = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      enumerationValues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The built-in slot type used as a parent for the slot type.
getSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\GetSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@GetSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: GetSlotTypeResponse)

-- | Configuration information that extends the parent built-in slot type.
getSlotTypeResponse_slotTypeConfigurations :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe [SlotTypeConfiguration])
getSlotTypeResponse_slotTypeConfigurations = Lens.lens (\GetSlotTypeResponse' {slotTypeConfigurations} -> slotTypeConfigurations) (\s@GetSlotTypeResponse' {} a -> s {slotTypeConfigurations = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | Checksum of the @$LATEST@ version of the slot type.
getSlotTypeResponse_checksum :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_checksum = Lens.lens (\GetSlotTypeResponse' {checksum} -> checksum) (\s@GetSlotTypeResponse' {} a -> s {checksum = a} :: GetSlotTypeResponse)

-- | The strategy that Amazon Lex uses to determine the value of the slot.
-- For more information, see PutSlotType.
getSlotTypeResponse_valueSelectionStrategy :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe SlotValueSelectionStrategy)
getSlotTypeResponse_valueSelectionStrategy = Lens.lens (\GetSlotTypeResponse' {valueSelectionStrategy} -> valueSelectionStrategy) (\s@GetSlotTypeResponse' {} a -> s {valueSelectionStrategy = a} :: GetSlotTypeResponse)

-- | The date that the slot type was created.
getSlotTypeResponse_createdDate :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
getSlotTypeResponse_createdDate = Lens.lens (\GetSlotTypeResponse' {createdDate} -> createdDate) (\s@GetSlotTypeResponse' {} a -> s {createdDate = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the slot type.
getSlotTypeResponse_name :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_name = Lens.lens (\GetSlotTypeResponse' {name} -> name) (\s@GetSlotTypeResponse' {} a -> s {name = a} :: GetSlotTypeResponse)

-- | The version of the slot type.
getSlotTypeResponse_version :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_version = Lens.lens (\GetSlotTypeResponse' {version} -> version) (\s@GetSlotTypeResponse' {} a -> s {version = a} :: GetSlotTypeResponse)

-- | The date that the slot type was updated. When you create a resource, the
-- creation date and last update date are the same.
getSlotTypeResponse_lastUpdatedDate :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
getSlotTypeResponse_lastUpdatedDate = Lens.lens (\GetSlotTypeResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetSlotTypeResponse' {} a -> s {lastUpdatedDate = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Core._Time

-- | A description of the slot type.
getSlotTypeResponse_description :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_description = Lens.lens (\GetSlotTypeResponse' {description} -> description) (\s@GetSlotTypeResponse' {} a -> s {description = a} :: GetSlotTypeResponse)

-- | A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
getSlotTypeResponse_enumerationValues :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe [EnumerationValue])
getSlotTypeResponse_enumerationValues = Lens.lens (\GetSlotTypeResponse' {enumerationValues} -> enumerationValues) (\s@GetSlotTypeResponse' {} a -> s {enumerationValues = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSlotTypeResponse_httpStatus :: Lens.Lens' GetSlotTypeResponse Prelude.Int
getSlotTypeResponse_httpStatus = Lens.lens (\GetSlotTypeResponse' {httpStatus} -> httpStatus) (\s@GetSlotTypeResponse' {} a -> s {httpStatus = a} :: GetSlotTypeResponse)

instance Prelude.NFData GetSlotTypeResponse
