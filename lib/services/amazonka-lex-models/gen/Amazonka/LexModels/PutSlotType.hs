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
-- Module      : Amazonka.LexModels.PutSlotType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom slot type or replaces an existing custom slot type.
--
-- To create a custom slot type, specify a name for the slot type and a set
-- of enumeration values, which are the values that a slot of this type can
-- assume. For more information, see how-it-works.
--
-- If you specify the name of an existing slot type, the fields in the
-- request replace the existing values in the @$LATEST@ version of the slot
-- type. Amazon Lex removes the fields that you don\'t provide in the
-- request. If you don\'t specify required fields, Amazon Lex throws an
-- exception. When you update the @$LATEST@ version of a slot type, if a
-- bot uses the @$LATEST@ version of an intent that contains the slot type,
-- the bot\'s @status@ field is set to @NOT_BUILT@.
--
-- This operation requires permissions for the @lex:PutSlotType@ action.
module Amazonka.LexModels.PutSlotType
  ( -- * Creating a Request
    PutSlotType (..),
    newPutSlotType,

    -- * Request Lenses
    putSlotType_checksum,
    putSlotType_createVersion,
    putSlotType_description,
    putSlotType_enumerationValues,
    putSlotType_parentSlotTypeSignature,
    putSlotType_slotTypeConfigurations,
    putSlotType_valueSelectionStrategy,
    putSlotType_name,

    -- * Destructuring the Response
    PutSlotTypeResponse (..),
    newPutSlotTypeResponse,

    -- * Response Lenses
    putSlotTypeResponse_checksum,
    putSlotTypeResponse_createVersion,
    putSlotTypeResponse_createdDate,
    putSlotTypeResponse_description,
    putSlotTypeResponse_enumerationValues,
    putSlotTypeResponse_lastUpdatedDate,
    putSlotTypeResponse_name,
    putSlotTypeResponse_parentSlotTypeSignature,
    putSlotTypeResponse_slotTypeConfigurations,
    putSlotTypeResponse_valueSelectionStrategy,
    putSlotTypeResponse_version,
    putSlotTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSlotType' smart constructor.
data PutSlotType = PutSlotType'
  { -- | Identifies a specific revision of the @$LATEST@ version.
    --
    -- When you create a new slot type, leave the @checksum@ field blank. If
    -- you specify a checksum you get a @BadRequestException@ exception.
    --
    -- When you want to update a slot type, set the @checksum@ field to the
    -- checksum of the most recent revision of the @$LATEST@ version. If you
    -- don\'t specify the @ checksum@ field, or if the checksum does not match
    -- the @$LATEST@ version, you get a @PreconditionFailedException@
    -- exception.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@ a new numbered version of the slot type is created.
    -- This is the same as calling the @CreateSlotTypeVersion@ operation. If
    -- you do not specify @createVersion@, the default is @false@.
    createVersion :: Prelude.Maybe Prelude.Bool,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the
    -- slot type can take. Each value can have a list of @synonyms@, which are
    -- additional values that help train the machine learning model about the
    -- values that it resolves for a slot.
    --
    -- A regular expression slot type doesn\'t require enumeration values. All
    -- other slot types require a list of enumeration values.
    --
    -- When Amazon Lex resolves a slot value, it generates a resolution list
    -- that contains up to five possible values for the slot. If you are using
    -- a Lambda function, this resolution list is passed to the function. If
    -- you are not using a Lambda function you can choose to return the value
    -- that the user entered or the first value in the resolution list as the
    -- slot value. The @valueSelectionStrategy@ field indicates the option to
    -- use.
    enumerationValues :: Prelude.Maybe [EnumerationValue],
    -- | The built-in slot type used as the parent of the slot type. When you
    -- define a parent slot type, the new slot type has all of the same
    -- configuration as the parent.
    --
    -- Only @AMAZON.AlphaNumeric@ is supported.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | Configuration information that extends the parent built-in slot type.
    -- The configuration is added to the settings for the parent slot type.
    slotTypeConfigurations :: Prelude.Maybe [SlotTypeConfiguration],
    -- | Determines the slot resolution strategy that Amazon Lex uses to return
    -- slot type values. The field can be set to one of the following values:
    --
    -- -   @ORIGINAL_VALUE@ - Returns the value entered by the user, if the
    --     user value is similar to the slot value.
    --
    -- -   @TOP_RESOLUTION@ - If there is a resolution list for the slot,
    --     return the first value in the resolution list as the slot type
    --     value. If there is no resolution list, null is returned.
    --
    -- If you don\'t specify the @valueSelectionStrategy@, the default is
    -- @ORIGINAL_VALUE@.
    valueSelectionStrategy :: Prelude.Maybe SlotValueSelectionStrategy,
    -- | The name of the slot type. The name is /not/ case sensitive.
    --
    -- The name can\'t match a built-in slot type name, or a built-in slot type
    -- name with \"AMAZON.\" removed. For example, because there is a built-in
    -- slot type called @AMAZON.DATE@, you can\'t create a custom slot type
    -- called @DATE@.
    --
    -- For a list of built-in slot types, see
    -- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
    -- in the /Alexa Skills Kit/.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'putSlotType_checksum' - Identifies a specific revision of the @$LATEST@ version.
--
-- When you create a new slot type, leave the @checksum@ field blank. If
-- you specify a checksum you get a @BadRequestException@ exception.
--
-- When you want to update a slot type, set the @checksum@ field to the
-- checksum of the most recent revision of the @$LATEST@ version. If you
-- don\'t specify the @ checksum@ field, or if the checksum does not match
-- the @$LATEST@ version, you get a @PreconditionFailedException@
-- exception.
--
-- 'createVersion', 'putSlotType_createVersion' - When set to @true@ a new numbered version of the slot type is created.
-- This is the same as calling the @CreateSlotTypeVersion@ operation. If
-- you do not specify @createVersion@, the default is @false@.
--
-- 'description', 'putSlotType_description' - A description of the slot type.
--
-- 'enumerationValues', 'putSlotType_enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take. Each value can have a list of @synonyms@, which are
-- additional values that help train the machine learning model about the
-- values that it resolves for a slot.
--
-- A regular expression slot type doesn\'t require enumeration values. All
-- other slot types require a list of enumeration values.
--
-- When Amazon Lex resolves a slot value, it generates a resolution list
-- that contains up to five possible values for the slot. If you are using
-- a Lambda function, this resolution list is passed to the function. If
-- you are not using a Lambda function you can choose to return the value
-- that the user entered or the first value in the resolution list as the
-- slot value. The @valueSelectionStrategy@ field indicates the option to
-- use.
--
-- 'parentSlotTypeSignature', 'putSlotType_parentSlotTypeSignature' - The built-in slot type used as the parent of the slot type. When you
-- define a parent slot type, the new slot type has all of the same
-- configuration as the parent.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
--
-- 'slotTypeConfigurations', 'putSlotType_slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
-- The configuration is added to the settings for the parent slot type.
--
-- 'valueSelectionStrategy', 'putSlotType_valueSelectionStrategy' - Determines the slot resolution strategy that Amazon Lex uses to return
-- slot type values. The field can be set to one of the following values:
--
-- -   @ORIGINAL_VALUE@ - Returns the value entered by the user, if the
--     user value is similar to the slot value.
--
-- -   @TOP_RESOLUTION@ - If there is a resolution list for the slot,
--     return the first value in the resolution list as the slot type
--     value. If there is no resolution list, null is returned.
--
-- If you don\'t specify the @valueSelectionStrategy@, the default is
-- @ORIGINAL_VALUE@.
--
-- 'name', 'putSlotType_name' - The name of the slot type. The name is /not/ case sensitive.
--
-- The name can\'t match a built-in slot type name, or a built-in slot type
-- name with \"AMAZON.\" removed. For example, because there is a built-in
-- slot type called @AMAZON.DATE@, you can\'t create a custom slot type
-- called @DATE@.
--
-- For a list of built-in slot types, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
-- in the /Alexa Skills Kit/.
newPutSlotType ::
  -- | 'name'
  Prelude.Text ->
  PutSlotType
newPutSlotType pName_ =
  PutSlotType'
    { checksum = Prelude.Nothing,
      createVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      enumerationValues = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      slotTypeConfigurations = Prelude.Nothing,
      valueSelectionStrategy = Prelude.Nothing,
      name = pName_
    }

-- | Identifies a specific revision of the @$LATEST@ version.
--
-- When you create a new slot type, leave the @checksum@ field blank. If
-- you specify a checksum you get a @BadRequestException@ exception.
--
-- When you want to update a slot type, set the @checksum@ field to the
-- checksum of the most recent revision of the @$LATEST@ version. If you
-- don\'t specify the @ checksum@ field, or if the checksum does not match
-- the @$LATEST@ version, you get a @PreconditionFailedException@
-- exception.
putSlotType_checksum :: Lens.Lens' PutSlotType (Prelude.Maybe Prelude.Text)
putSlotType_checksum = Lens.lens (\PutSlotType' {checksum} -> checksum) (\s@PutSlotType' {} a -> s {checksum = a} :: PutSlotType)

-- | When set to @true@ a new numbered version of the slot type is created.
-- This is the same as calling the @CreateSlotTypeVersion@ operation. If
-- you do not specify @createVersion@, the default is @false@.
putSlotType_createVersion :: Lens.Lens' PutSlotType (Prelude.Maybe Prelude.Bool)
putSlotType_createVersion = Lens.lens (\PutSlotType' {createVersion} -> createVersion) (\s@PutSlotType' {} a -> s {createVersion = a} :: PutSlotType)

-- | A description of the slot type.
putSlotType_description :: Lens.Lens' PutSlotType (Prelude.Maybe Prelude.Text)
putSlotType_description = Lens.lens (\PutSlotType' {description} -> description) (\s@PutSlotType' {} a -> s {description = a} :: PutSlotType)

-- | A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take. Each value can have a list of @synonyms@, which are
-- additional values that help train the machine learning model about the
-- values that it resolves for a slot.
--
-- A regular expression slot type doesn\'t require enumeration values. All
-- other slot types require a list of enumeration values.
--
-- When Amazon Lex resolves a slot value, it generates a resolution list
-- that contains up to five possible values for the slot. If you are using
-- a Lambda function, this resolution list is passed to the function. If
-- you are not using a Lambda function you can choose to return the value
-- that the user entered or the first value in the resolution list as the
-- slot value. The @valueSelectionStrategy@ field indicates the option to
-- use.
putSlotType_enumerationValues :: Lens.Lens' PutSlotType (Prelude.Maybe [EnumerationValue])
putSlotType_enumerationValues = Lens.lens (\PutSlotType' {enumerationValues} -> enumerationValues) (\s@PutSlotType' {} a -> s {enumerationValues = a} :: PutSlotType) Prelude.. Lens.mapping Lens.coerced

-- | The built-in slot type used as the parent of the slot type. When you
-- define a parent slot type, the new slot type has all of the same
-- configuration as the parent.
--
-- Only @AMAZON.AlphaNumeric@ is supported.
putSlotType_parentSlotTypeSignature :: Lens.Lens' PutSlotType (Prelude.Maybe Prelude.Text)
putSlotType_parentSlotTypeSignature = Lens.lens (\PutSlotType' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@PutSlotType' {} a -> s {parentSlotTypeSignature = a} :: PutSlotType)

-- | Configuration information that extends the parent built-in slot type.
-- The configuration is added to the settings for the parent slot type.
putSlotType_slotTypeConfigurations :: Lens.Lens' PutSlotType (Prelude.Maybe [SlotTypeConfiguration])
putSlotType_slotTypeConfigurations = Lens.lens (\PutSlotType' {slotTypeConfigurations} -> slotTypeConfigurations) (\s@PutSlotType' {} a -> s {slotTypeConfigurations = a} :: PutSlotType) Prelude.. Lens.mapping Lens.coerced

-- | Determines the slot resolution strategy that Amazon Lex uses to return
-- slot type values. The field can be set to one of the following values:
--
-- -   @ORIGINAL_VALUE@ - Returns the value entered by the user, if the
--     user value is similar to the slot value.
--
-- -   @TOP_RESOLUTION@ - If there is a resolution list for the slot,
--     return the first value in the resolution list as the slot type
--     value. If there is no resolution list, null is returned.
--
-- If you don\'t specify the @valueSelectionStrategy@, the default is
-- @ORIGINAL_VALUE@.
putSlotType_valueSelectionStrategy :: Lens.Lens' PutSlotType (Prelude.Maybe SlotValueSelectionStrategy)
putSlotType_valueSelectionStrategy = Lens.lens (\PutSlotType' {valueSelectionStrategy} -> valueSelectionStrategy) (\s@PutSlotType' {} a -> s {valueSelectionStrategy = a} :: PutSlotType)

-- | The name of the slot type. The name is /not/ case sensitive.
--
-- The name can\'t match a built-in slot type name, or a built-in slot type
-- name with \"AMAZON.\" removed. For example, because there is a built-in
-- slot type called @AMAZON.DATE@, you can\'t create a custom slot type
-- called @DATE@.
--
-- For a list of built-in slot types, see
-- <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/slot-type-reference Slot Type Reference>
-- in the /Alexa Skills Kit/.
putSlotType_name :: Lens.Lens' PutSlotType Prelude.Text
putSlotType_name = Lens.lens (\PutSlotType' {name} -> name) (\s@PutSlotType' {} a -> s {name = a} :: PutSlotType)

instance Core.AWSRequest PutSlotType where
  type AWSResponse PutSlotType = PutSlotTypeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSlotTypeResponse'
            Prelude.<$> (x Data..?> "checksum")
            Prelude.<*> (x Data..?> "createVersion")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> ( x Data..?> "enumerationValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "lastUpdatedDate")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "parentSlotTypeSignature")
            Prelude.<*> ( x Data..?> "slotTypeConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "valueSelectionStrategy")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSlotType where
  hashWithSalt _salt PutSlotType' {..} =
    _salt `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` createVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enumerationValues
      `Prelude.hashWithSalt` parentSlotTypeSignature
      `Prelude.hashWithSalt` slotTypeConfigurations
      `Prelude.hashWithSalt` valueSelectionStrategy
      `Prelude.hashWithSalt` name

instance Prelude.NFData PutSlotType where
  rnf PutSlotType' {..} =
    Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf createVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enumerationValues
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf slotTypeConfigurations
      `Prelude.seq` Prelude.rnf valueSelectionStrategy
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders PutSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutSlotType where
  toJSON PutSlotType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("checksum" Data..=) Prelude.<$> checksum,
            ("createVersion" Data..=) Prelude.<$> createVersion,
            ("description" Data..=) Prelude.<$> description,
            ("enumerationValues" Data..=)
              Prelude.<$> enumerationValues,
            ("parentSlotTypeSignature" Data..=)
              Prelude.<$> parentSlotTypeSignature,
            ("slotTypeConfigurations" Data..=)
              Prelude.<$> slotTypeConfigurations,
            ("valueSelectionStrategy" Data..=)
              Prelude.<$> valueSelectionStrategy
          ]
      )

instance Data.ToPath PutSlotType where
  toPath PutSlotType' {..} =
    Prelude.mconcat
      ["/slottypes/", Data.toBS name, "/versions/$LATEST"]

instance Data.ToQuery PutSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSlotTypeResponse' smart constructor.
data PutSlotTypeResponse = PutSlotTypeResponse'
  { -- | Checksum of the @$LATEST@ version of the slot type.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | @True@ if a new version of the slot type was created. If the
    -- @createVersion@ field was not specified in the request, the
    -- @createVersion@ field is set to false in the response.
    createVersion :: Prelude.Maybe Prelude.Bool,
    -- | The date that the slot type was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @EnumerationValue@ objects that defines the values that the
    -- slot type can take.
    enumerationValues :: Prelude.Maybe [EnumerationValue],
    -- | The date that the slot type was updated. When you create a slot type,
    -- the creation date and last update date are the same.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the slot type.
    name :: Prelude.Maybe Prelude.Text,
    -- | The built-in slot type used as the parent of the slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | Configuration information that extends the parent built-in slot type.
    slotTypeConfigurations :: Prelude.Maybe [SlotTypeConfiguration],
    -- | The slot resolution strategy that Amazon Lex uses to determine the value
    -- of the slot. For more information, see PutSlotType.
    valueSelectionStrategy :: Prelude.Maybe SlotValueSelectionStrategy,
    -- | The version of the slot type. For a new slot type, the version is always
    -- @$LATEST@.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksum', 'putSlotTypeResponse_checksum' - Checksum of the @$LATEST@ version of the slot type.
--
-- 'createVersion', 'putSlotTypeResponse_createVersion' - @True@ if a new version of the slot type was created. If the
-- @createVersion@ field was not specified in the request, the
-- @createVersion@ field is set to false in the response.
--
-- 'createdDate', 'putSlotTypeResponse_createdDate' - The date that the slot type was created.
--
-- 'description', 'putSlotTypeResponse_description' - A description of the slot type.
--
-- 'enumerationValues', 'putSlotTypeResponse_enumerationValues' - A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
--
-- 'lastUpdatedDate', 'putSlotTypeResponse_lastUpdatedDate' - The date that the slot type was updated. When you create a slot type,
-- the creation date and last update date are the same.
--
-- 'name', 'putSlotTypeResponse_name' - The name of the slot type.
--
-- 'parentSlotTypeSignature', 'putSlotTypeResponse_parentSlotTypeSignature' - The built-in slot type used as the parent of the slot type.
--
-- 'slotTypeConfigurations', 'putSlotTypeResponse_slotTypeConfigurations' - Configuration information that extends the parent built-in slot type.
--
-- 'valueSelectionStrategy', 'putSlotTypeResponse_valueSelectionStrategy' - The slot resolution strategy that Amazon Lex uses to determine the value
-- of the slot. For more information, see PutSlotType.
--
-- 'version', 'putSlotTypeResponse_version' - The version of the slot type. For a new slot type, the version is always
-- @$LATEST@.
--
-- 'httpStatus', 'putSlotTypeResponse_httpStatus' - The response's http status code.
newPutSlotTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSlotTypeResponse
newPutSlotTypeResponse pHttpStatus_ =
  PutSlotTypeResponse'
    { checksum = Prelude.Nothing,
      createVersion = Prelude.Nothing,
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
putSlotTypeResponse_checksum :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.Text)
putSlotTypeResponse_checksum = Lens.lens (\PutSlotTypeResponse' {checksum} -> checksum) (\s@PutSlotTypeResponse' {} a -> s {checksum = a} :: PutSlotTypeResponse)

-- | @True@ if a new version of the slot type was created. If the
-- @createVersion@ field was not specified in the request, the
-- @createVersion@ field is set to false in the response.
putSlotTypeResponse_createVersion :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.Bool)
putSlotTypeResponse_createVersion = Lens.lens (\PutSlotTypeResponse' {createVersion} -> createVersion) (\s@PutSlotTypeResponse' {} a -> s {createVersion = a} :: PutSlotTypeResponse)

-- | The date that the slot type was created.
putSlotTypeResponse_createdDate :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
putSlotTypeResponse_createdDate = Lens.lens (\PutSlotTypeResponse' {createdDate} -> createdDate) (\s@PutSlotTypeResponse' {} a -> s {createdDate = a} :: PutSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the slot type.
putSlotTypeResponse_description :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.Text)
putSlotTypeResponse_description = Lens.lens (\PutSlotTypeResponse' {description} -> description) (\s@PutSlotTypeResponse' {} a -> s {description = a} :: PutSlotTypeResponse)

-- | A list of @EnumerationValue@ objects that defines the values that the
-- slot type can take.
putSlotTypeResponse_enumerationValues :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe [EnumerationValue])
putSlotTypeResponse_enumerationValues = Lens.lens (\PutSlotTypeResponse' {enumerationValues} -> enumerationValues) (\s@PutSlotTypeResponse' {} a -> s {enumerationValues = a} :: PutSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date that the slot type was updated. When you create a slot type,
-- the creation date and last update date are the same.
putSlotTypeResponse_lastUpdatedDate :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.UTCTime)
putSlotTypeResponse_lastUpdatedDate = Lens.lens (\PutSlotTypeResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@PutSlotTypeResponse' {} a -> s {lastUpdatedDate = a} :: PutSlotTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the slot type.
putSlotTypeResponse_name :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.Text)
putSlotTypeResponse_name = Lens.lens (\PutSlotTypeResponse' {name} -> name) (\s@PutSlotTypeResponse' {} a -> s {name = a} :: PutSlotTypeResponse)

-- | The built-in slot type used as the parent of the slot type.
putSlotTypeResponse_parentSlotTypeSignature :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.Text)
putSlotTypeResponse_parentSlotTypeSignature = Lens.lens (\PutSlotTypeResponse' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@PutSlotTypeResponse' {} a -> s {parentSlotTypeSignature = a} :: PutSlotTypeResponse)

-- | Configuration information that extends the parent built-in slot type.
putSlotTypeResponse_slotTypeConfigurations :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe [SlotTypeConfiguration])
putSlotTypeResponse_slotTypeConfigurations = Lens.lens (\PutSlotTypeResponse' {slotTypeConfigurations} -> slotTypeConfigurations) (\s@PutSlotTypeResponse' {} a -> s {slotTypeConfigurations = a} :: PutSlotTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The slot resolution strategy that Amazon Lex uses to determine the value
-- of the slot. For more information, see PutSlotType.
putSlotTypeResponse_valueSelectionStrategy :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe SlotValueSelectionStrategy)
putSlotTypeResponse_valueSelectionStrategy = Lens.lens (\PutSlotTypeResponse' {valueSelectionStrategy} -> valueSelectionStrategy) (\s@PutSlotTypeResponse' {} a -> s {valueSelectionStrategy = a} :: PutSlotTypeResponse)

-- | The version of the slot type. For a new slot type, the version is always
-- @$LATEST@.
putSlotTypeResponse_version :: Lens.Lens' PutSlotTypeResponse (Prelude.Maybe Prelude.Text)
putSlotTypeResponse_version = Lens.lens (\PutSlotTypeResponse' {version} -> version) (\s@PutSlotTypeResponse' {} a -> s {version = a} :: PutSlotTypeResponse)

-- | The response's http status code.
putSlotTypeResponse_httpStatus :: Lens.Lens' PutSlotTypeResponse Prelude.Int
putSlotTypeResponse_httpStatus = Lens.lens (\PutSlotTypeResponse' {httpStatus} -> httpStatus) (\s@PutSlotTypeResponse' {} a -> s {httpStatus = a} :: PutSlotTypeResponse)

instance Prelude.NFData PutSlotTypeResponse where
  rnf PutSlotTypeResponse' {..} =
    Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf createVersion
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
