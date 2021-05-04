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
-- Module      : Network.AWS.LexModels.CreateSlotTypeVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a slot type based on the @$LATEST@ version of
-- the specified slot type. If the @$LATEST@ version of this resource has
-- not changed since the last version that you created, Amazon Lex doesn\'t
-- create a new version. It returns the last version that you created.
--
-- You can update only the @$LATEST@ version of a slot type. You can\'t
-- update the numbered versions that you create with the
-- @CreateSlotTypeVersion@ operation.
--
-- When you create a version of a slot type, Amazon Lex sets the version to
-- 1. Subsequent versions increment by 1. For more information, see
-- versioning-intro.
--
-- This operation requires permissions for the @lex:CreateSlotTypeVersion@
-- action.
module Network.AWS.LexModels.CreateSlotTypeVersion
  ( -- * Creating a Request
    CreateSlotTypeVersion (..),
    newCreateSlotTypeVersion,

    -- * Request Lenses
    createSlotTypeVersion_checksum,
    createSlotTypeVersion_name,

    -- * Destructuring the Response
    CreateSlotTypeVersionResponse (..),
    newCreateSlotTypeVersionResponse,

    -- * Response Lenses
    createSlotTypeVersionResponse_slotTypeConfigurations,
    createSlotTypeVersionResponse_createdDate,
    createSlotTypeVersionResponse_enumerationValues,
    createSlotTypeVersionResponse_lastUpdatedDate,
    createSlotTypeVersionResponse_valueSelectionStrategy,
    createSlotTypeVersionResponse_version,
    createSlotTypeVersionResponse_name,
    createSlotTypeVersionResponse_parentSlotTypeSignature,
    createSlotTypeVersionResponse_description,
    createSlotTypeVersionResponse_checksum,
    createSlotTypeVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSlotTypeVersion' smart constructor.
data CreateSlotTypeVersion = CreateSlotTypeVersion'
  { -- | Checksum for the @$LATEST@ version of the slot type that you want to
    -- publish. If you specify a checksum and the @$LATEST@ version of the slot
    -- type has a different checksum, Amazon Lex returns a
    -- @PreconditionFailedException@ exception and doesn\'t publish the new
    -- version. If you don\'t specify a checksum, Amazon Lex publishes the
    -- @$LATEST@ version.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The name of the slot type that you want to create a new version for. The
    -- name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSlotTypeVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'createSlotTypeVersion_checksum' - Checksum for the @$LATEST@ version of the slot type that you want to
-- publish. If you specify a checksum and the @$LATEST@ version of the slot
-- type has a different checksum, Amazon Lex returns a
-- @PreconditionFailedException@ exception and doesn\'t publish the new
-- version. If you don\'t specify a checksum, Amazon Lex publishes the
-- @$LATEST@ version.
--
-- 'name', 'createSlotTypeVersion_name' - The name of the slot type that you want to create a new version for. The
-- name is case sensitive.
newCreateSlotTypeVersion ::
  -- | 'name'
  Prelude.Text ->
  CreateSlotTypeVersion
newCreateSlotTypeVersion pName_ =
  CreateSlotTypeVersion'
    { checksum = Prelude.Nothing,
      name = pName_
    }

-- | Checksum for the @$LATEST@ version of the slot type that you want to
-- publish. If you specify a checksum and the @$LATEST@ version of the slot
-- type has a different checksum, Amazon Lex returns a
-- @PreconditionFailedException@ exception and doesn\'t publish the new
-- version. If you don\'t specify a checksum, Amazon Lex publishes the
-- @$LATEST@ version.
createSlotTypeVersion_checksum :: Lens.Lens' CreateSlotTypeVersion (Prelude.Maybe Prelude.Text)
createSlotTypeVersion_checksum = Lens.lens (\CreateSlotTypeVersion' {checksum} -> checksum) (\s@CreateSlotTypeVersion' {} a -> s {checksum = a} :: CreateSlotTypeVersion)

-- | The name of the slot type that you want to create a new version for. The
-- name is case sensitive.
createSlotTypeVersion_name :: Lens.Lens' CreateSlotTypeVersion Prelude.Text
createSlotTypeVersion_name = Lens.lens (\CreateSlotTypeVersion' {name} -> name) (\s@CreateSlotTypeVersion' {} a -> s {name = a} :: CreateSlotTypeVersion)

instance Prelude.AWSRequest CreateSlotTypeVersion where
  type
    Rs CreateSlotTypeVersion =
      CreateSlotTypeVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSlotTypeVersionResponse'
            Prelude.<$> ( x Prelude..?> "slotTypeConfigurations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "createdDate")
            Prelude.<*> ( x Prelude..?> "enumerationValues"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "lastUpdatedDate")
            Prelude.<*> (x Prelude..?> "valueSelectionStrategy")
            Prelude.<*> (x Prelude..?> "version")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "parentSlotTypeSignature")
            Prelude.<*> (x Prelude..?> "description")
            Prelude.<*> (x Prelude..?> "checksum")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSlotTypeVersion

instance Prelude.NFData CreateSlotTypeVersion

instance Prelude.ToHeaders CreateSlotTypeVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateSlotTypeVersion where
  toJSON CreateSlotTypeVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("checksum" Prelude..=) Prelude.<$> checksum]
      )

instance Prelude.ToPath CreateSlotTypeVersion where
  toPath CreateSlotTypeVersion' {..} =
    Prelude.mconcat
      ["/slottypes/", Prelude.toBS name, "/versions"]

instance Prelude.ToQuery CreateSlotTypeVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSlotTypeVersionResponse' smart constructor.
data CreateSlotTypeVersionResponse = CreateSlotTypeVersionResponse'
  { -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Prelude.Maybe [SlotTypeConfiguration],
    -- | The date that the slot type was created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
    -- | A list of @EnumerationValue@ objects that defines the values that the
    -- slot type can take.
    enumerationValues :: Prelude.Maybe [EnumerationValue],
    -- | The date that the slot type was updated. When you create a resource, the
    -- creation date and last update date are the same.
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The strategy that Amazon Lex uses to determine the value of the slot.
    -- For more information, see PutSlotType.
    valueSelectionStrategy :: Prelude.Maybe SlotValueSelectionStrategy,
    -- | The version assigned to the new slot type version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the slot type.
    name :: Prelude.Maybe Prelude.Text,
    -- | The built-in slot type used a the parent of the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checksum of the @$LATEST@ version of the slot type.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSlotTypeVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotTypeConfigurations', 'createSlotTypeVersionResponse_slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
--
-- 'createdDate', 'createSlotTypeVersionResponse_createdDate' - The date that the slot type was created.
--
-- 'enumerationValues', 'createSlotTypeVersionResponse_enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
--
-- 'lastUpdatedDate', 'createSlotTypeVersionResponse_lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the
-- creation date and last update date are the same.
--
-- 'valueSelectionStrategy', 'createSlotTypeVersionResponse_valueSelectionStrategy' - The strategy that Amazon Lex uses to determine the value of the slot.
-- For more information, see PutSlotType.
--
-- 'version', 'createSlotTypeVersionResponse_version' - The version assigned to the new slot type version.
--
-- 'name', 'createSlotTypeVersionResponse_name' - The name of the slot type.
--
-- 'parentSlotTypeSignature', 'createSlotTypeVersionResponse_parentSlotTypeSignature' - The built-in slot type used a the parent of the slot type.
--
-- 'description', 'createSlotTypeVersionResponse_description' - A description of the slot type.
--
-- 'checksum', 'createSlotTypeVersionResponse_checksum' - Checksum of the @$LATEST@ version of the slot type.
--
-- 'httpStatus', 'createSlotTypeVersionResponse_httpStatus' - The response's http status code.
newCreateSlotTypeVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlotTypeVersionResponse
newCreateSlotTypeVersionResponse pHttpStatus_ =
  CreateSlotTypeVersionResponse'
    { slotTypeConfigurations =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      enumerationValues = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      valueSelectionStrategy = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      description = Prelude.Nothing,
      checksum = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Configuration information that extends the parent built-in slot type.
createSlotTypeVersionResponse_slotTypeConfigurations :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe [SlotTypeConfiguration])
createSlotTypeVersionResponse_slotTypeConfigurations = Lens.lens (\CreateSlotTypeVersionResponse' {slotTypeConfigurations} -> slotTypeConfigurations) (\s@CreateSlotTypeVersionResponse' {} a -> s {slotTypeConfigurations = a} :: CreateSlotTypeVersionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The date that the slot type was created.
createSlotTypeVersionResponse_createdDate :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.UTCTime)
createSlotTypeVersionResponse_createdDate = Lens.lens (\CreateSlotTypeVersionResponse' {createdDate} -> createdDate) (\s@CreateSlotTypeVersionResponse' {} a -> s {createdDate = a} :: CreateSlotTypeVersionResponse) Prelude.. Lens.mapping Prelude._Time

-- | A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
createSlotTypeVersionResponse_enumerationValues :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe [EnumerationValue])
createSlotTypeVersionResponse_enumerationValues = Lens.lens (\CreateSlotTypeVersionResponse' {enumerationValues} -> enumerationValues) (\s@CreateSlotTypeVersionResponse' {} a -> s {enumerationValues = a} :: CreateSlotTypeVersionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The date that the slot type was updated. When you create a resource, the
-- creation date and last update date are the same.
createSlotTypeVersionResponse_lastUpdatedDate :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.UTCTime)
createSlotTypeVersionResponse_lastUpdatedDate = Lens.lens (\CreateSlotTypeVersionResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@CreateSlotTypeVersionResponse' {} a -> s {lastUpdatedDate = a} :: CreateSlotTypeVersionResponse) Prelude.. Lens.mapping Prelude._Time

-- | The strategy that Amazon Lex uses to determine the value of the slot.
-- For more information, see PutSlotType.
createSlotTypeVersionResponse_valueSelectionStrategy :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe SlotValueSelectionStrategy)
createSlotTypeVersionResponse_valueSelectionStrategy = Lens.lens (\CreateSlotTypeVersionResponse' {valueSelectionStrategy} -> valueSelectionStrategy) (\s@CreateSlotTypeVersionResponse' {} a -> s {valueSelectionStrategy = a} :: CreateSlotTypeVersionResponse)

-- | The version assigned to the new slot type version.
createSlotTypeVersionResponse_version :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.Text)
createSlotTypeVersionResponse_version = Lens.lens (\CreateSlotTypeVersionResponse' {version} -> version) (\s@CreateSlotTypeVersionResponse' {} a -> s {version = a} :: CreateSlotTypeVersionResponse)

-- | The name of the slot type.
createSlotTypeVersionResponse_name :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.Text)
createSlotTypeVersionResponse_name = Lens.lens (\CreateSlotTypeVersionResponse' {name} -> name) (\s@CreateSlotTypeVersionResponse' {} a -> s {name = a} :: CreateSlotTypeVersionResponse)

-- | The built-in slot type used a the parent of the slot type.
createSlotTypeVersionResponse_parentSlotTypeSignature :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.Text)
createSlotTypeVersionResponse_parentSlotTypeSignature = Lens.lens (\CreateSlotTypeVersionResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@CreateSlotTypeVersionResponse' {} a -> s {parentSlotTypeSignature = a} :: CreateSlotTypeVersionResponse)

-- | A description of the slot type.
createSlotTypeVersionResponse_description :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.Text)
createSlotTypeVersionResponse_description = Lens.lens (\CreateSlotTypeVersionResponse' {description} -> description) (\s@CreateSlotTypeVersionResponse' {} a -> s {description = a} :: CreateSlotTypeVersionResponse)

-- | Checksum of the @$LATEST@ version of the slot type.
createSlotTypeVersionResponse_checksum :: Lens.Lens' CreateSlotTypeVersionResponse (Prelude.Maybe Prelude.Text)
createSlotTypeVersionResponse_checksum = Lens.lens (\CreateSlotTypeVersionResponse' {checksum} -> checksum) (\s@CreateSlotTypeVersionResponse' {} a -> s {checksum = a} :: CreateSlotTypeVersionResponse)

-- | The response's http status code.
createSlotTypeVersionResponse_httpStatus :: Lens.Lens' CreateSlotTypeVersionResponse Prelude.Int
createSlotTypeVersionResponse_httpStatus = Lens.lens (\CreateSlotTypeVersionResponse' {httpStatus} -> httpStatus) (\s@CreateSlotTypeVersionResponse' {} a -> s {httpStatus = a} :: CreateSlotTypeVersionResponse)

instance Prelude.NFData CreateSlotTypeVersionResponse
