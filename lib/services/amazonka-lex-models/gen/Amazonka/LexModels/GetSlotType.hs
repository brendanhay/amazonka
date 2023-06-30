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
-- Module      : Amazonka.LexModels.GetSlotType
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.LexModels.GetSlotType
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
    getSlotTypeResponse_checksum,
    getSlotTypeResponse_createdDate,
    getSlotTypeResponse_description,
    getSlotTypeResponse_enumerationValues,
    getSlotTypeResponse_lastUpdatedDate,
    getSlotTypeResponse_name,
    getSlotTypeResponse_parentSlotTypeSignature,
    getSlotTypeResponse_slotTypeConfigurations,
    getSlotTypeResponse_valueSelectionStrategy,
    getSlotTypeResponse_version,
    getSlotTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSlotTypeResponse'
            Prelude.<$> (x Data..?> "checksum")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> ( x
                            Data..?> "enumerationValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "lastUpdatedDate")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "parentSlotTypeSignature")
            Prelude.<*> ( x
                            Data..?> "slotTypeConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "valueSelectionStrategy")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSlotType where
  hashWithSalt _salt GetSlotType' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData GetSlotType where
  rnf GetSlotType' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders GetSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSlotType where
  toPath GetSlotType' {..} =
    Prelude.mconcat
      [ "/slottypes/",
        Data.toBS name,
        "/versions/",
        Data.toBS version
      ]

instance Data.ToQuery GetSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSlotTypeResponse' smart constructor.
data GetSlotTypeResponse = GetSlotTypeResponse'
  { -- | Checksum of the @$LATEST@ version of the slot type.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The date that the slot type was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the
    -- slot type can take.
    enumerationValues :: Prelude.Maybe [EnumerationValue],
    -- | The date that the slot type was updated. When you create a resource, the
    -- creation date and last update date are the same.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the slot type.
    name :: Prelude.Maybe Prelude.Text,
    -- | The built-in slot type used as a parent for the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Prelude.Maybe [SlotTypeConfiguration],
    -- | The strategy that Amazon Lex uses to determine the value of the slot.
    -- For more information, see PutSlotType.
    valueSelectionStrategy :: Prelude.Maybe SlotValueSelectionStrategy,
    -- | The version of the slot type.
    version :: Prelude.Maybe Prelude.Text,
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
-- 'checksum', 'getSlotTypeResponse_checksum' - Checksum of the @$LATEST@ version of the slot type.
--
-- 'createdDate', 'getSlotTypeResponse_createdDate' - The date that the slot type was created.
--
-- 'description', 'getSlotTypeResponse_description' - A description of the slot type.
--
-- 'enumerationValues', 'getSlotTypeResponse_enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
--
-- 'lastUpdatedDate', 'getSlotTypeResponse_lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the
-- creation date and last update date are the same.
--
-- 'name', 'getSlotTypeResponse_name' - The name of the slot type.
--
-- 'parentSlotTypeSignature', 'getSlotTypeResponse_parentSlotTypeSignature' - The built-in slot type used as a parent for the slot type.
--
-- 'slotTypeConfigurations', 'getSlotTypeResponse_slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
--
-- 'valueSelectionStrategy', 'getSlotTypeResponse_valueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot.
-- For more information, see PutSlotType.
--
-- 'version', 'getSlotTypeResponse_version' - The version of the slot type.
--
-- 'httpStatus', 'getSlotTypeResponse_httpStatus' - The response's http status code.
newGetSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSlotTypeResponse
newGetSlotTypeResponse pHttpStatus_ =
  GetSlotTypeResponse'
    { checksum = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      enumerationValues = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      slotTypeConfigurations = Prelude.Nothing,
      valueSelectionStrategy = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Checksum of the @$LATEST@ version of the slot type.
getSlotTypeResponse_checksum :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_checksum = Lens.lens (\GetSlotTypeResponse' {checksum} -> checksum) (\s@GetSlotTypeResponse' {} a -> s {checksum = a} :: GetSlotTypeResponse)

-- | The date that the slot type was created.
getSlotTypeResponse_createdDate :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
getSlotTypeResponse_createdDate = Lens.lens (\GetSlotTypeResponse' {createdDate} -> createdDate) (\s@GetSlotTypeResponse' {} a -> s {createdDate = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the slot type.
getSlotTypeResponse_description :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_description = Lens.lens (\GetSlotTypeResponse' {description} -> description) (\s@GetSlotTypeResponse' {} a -> s {description = a} :: GetSlotTypeResponse)

-- | A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
getSlotTypeResponse_enumerationValues :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe [EnumerationValue])
getSlotTypeResponse_enumerationValues = Lens.lens (\GetSlotTypeResponse' {enumerationValues} -> enumerationValues) (\s@GetSlotTypeResponse' {} a -> s {enumerationValues = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date that the slot type was updated. When you create a resource, the
-- creation date and last update date are the same.
getSlotTypeResponse_lastUpdatedDate :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
getSlotTypeResponse_lastUpdatedDate = Lens.lens (\GetSlotTypeResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetSlotTypeResponse' {} a -> s {lastUpdatedDate = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the slot type.
getSlotTypeResponse_name :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_name = Lens.lens (\GetSlotTypeResponse' {name} -> name) (\s@GetSlotTypeResponse' {} a -> s {name = a} :: GetSlotTypeResponse)

-- | The built-in slot type used as a parent for the slot type.
getSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\GetSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@GetSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: GetSlotTypeResponse)

-- | Configuration information that extends the parent built-in slot type.
getSlotTypeResponse_slotTypeConfigurations :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe [SlotTypeConfiguration])
getSlotTypeResponse_slotTypeConfigurations = Lens.lens (\GetSlotTypeResponse' {slotTypeConfigurations} -> slotTypeConfigurations) (\s@GetSlotTypeResponse' {} a -> s {slotTypeConfigurations = a} :: GetSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The strategy that Amazon Lex uses to determine the value of the slot.
-- For more information, see PutSlotType.
getSlotTypeResponse_valueSelectionStrategy :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe SlotValueSelectionStrategy)
getSlotTypeResponse_valueSelectionStrategy = Lens.lens (\GetSlotTypeResponse' {valueSelectionStrategy} -> valueSelectionStrategy) (\s@GetSlotTypeResponse' {} a -> s {valueSelectionStrategy = a} :: GetSlotTypeResponse)

-- | The version of the slot type.
getSlotTypeResponse_version :: Lens.Lens' GetSlotTypeResponse (Prelude.Maybe Prelude.Text)
getSlotTypeResponse_version = Lens.lens (\GetSlotTypeResponse' {version} -> version) (\s@GetSlotTypeResponse' {} a -> s {version = a} :: GetSlotTypeResponse)

-- | The response's http status code.
getSlotTypeResponse_httpStatus :: Lens.Lens' GetSlotTypeResponse Prelude.Int
getSlotTypeResponse_httpStatus = Lens.lens (\GetSlotTypeResponse' {httpStatus} -> httpStatus) (\s@GetSlotTypeResponse' {} a -> s {httpStatus = a} :: GetSlotTypeResponse)

instance Prelude.NFData GetSlotTypeResponse where
  rnf GetSlotTypeResponse' {..} =
    Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enumerationValues
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf slotTypeConfigurations
      `Prelude.seq` Prelude.rnf valueSelectionStrategy
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
