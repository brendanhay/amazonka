{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.Types.ByteMatchStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ByteMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FieldToMatch
import Amazonka.WAFV2.Types.PositionalConstraint
import Amazonka.WAFV2.Types.TextTransformation

-- | A rule statement that defines a string match search for WAF to apply to
-- web requests. The byte match statement provides the bytes to search for,
-- the location in requests that you want WAF to search, and other
-- settings. The bytes to search for are typically a string that
-- corresponds with ASCII characters. In the WAF console and the developer
-- guide, this is called a string match statement.
--
-- /See:/ 'newByteMatchStatement' smart constructor.
data ByteMatchStatement = ByteMatchStatement'
  { -- | A string value that you want WAF to search for. WAF searches only in the
    -- part of web requests that you designate for inspection in FieldToMatch.
    -- The maximum length of the value is 50 bytes.
    --
    -- Valid values depend on the component that you specify for inspection in
    -- @FieldToMatch@:
    --
    -- -   @Method@: The HTTP method that you want WAF to search for. This
    --     indicates the type of operation specified in the request.
    --
    -- -   @UriPath@: The value that you want WAF to search for in the URI
    --     path, for example, @\/images\/daily-ad.jpg@.
    --
    -- If @SearchString@ includes alphabetic characters A-Z and a-z, note that
    -- the value is case sensitive.
    --
    -- __If you\'re using the WAF API__
    --
    -- Specify a base64-encoded version of the value. The maximum length of the
    -- value before you base64-encode it is 50 bytes.
    --
    -- For example, suppose the value of @Type@ is @HEADER@ and the value of
    -- @Data@ is @User-Agent@. If you want to search the @User-Agent@ header
    -- for the value @BadBot@, you base64-encode @BadBot@ using MIME
    -- base64-encoding and include the resulting value, @QmFkQm90@, in the
    -- value of @SearchString@.
    --
    -- __If you\'re using the CLI or one of the Amazon Web Services SDKs__
    --
    -- The value that you want WAF to search for. The SDK automatically base64
    -- encodes the value.
    searchString :: Data.Base64,
    -- | The part of the web request that you want WAF to inspect.
    fieldToMatch :: FieldToMatch,
    -- | Text transformations eliminate some of the unusual formatting that
    -- attackers use in web requests in an effort to bypass detection. If you
    -- specify one or more transformations in a rule statement, WAF performs
    -- all transformations on the content of the request component identified
    -- by @FieldToMatch@, starting from the lowest priority setting, before
    -- inspecting the content for a match.
    textTransformations :: Prelude.NonEmpty TextTransformation,
    -- | The area within the portion of the web request that you want WAF to
    -- search for @SearchString@. Valid values include the following:
    --
    -- __CONTAINS__
    --
    -- The specified part of the web request must include the value of
    -- @SearchString@, but the location doesn\'t matter.
    --
    -- __CONTAINS_WORD__
    --
    -- The specified part of the web request must include the value of
    -- @SearchString@, and @SearchString@ must contain only alphanumeric
    -- characters or underscore (A-Z, a-z, 0-9, or _). In addition,
    -- @SearchString@ must be a word, which means that both of the following
    -- are true:
    --
    -- -   @SearchString@ is at the beginning of the specified part of the web
    --     request or is preceded by a character other than an alphanumeric
    --     character or underscore (_). Examples include the value of a header
    --     and @;BadBot@.
    --
    -- -   @SearchString@ is at the end of the specified part of the web
    --     request or is followed by a character other than an alphanumeric
    --     character or underscore (_), for example, @BadBot;@ and @-BadBot;@.
    --
    -- __EXACTLY__
    --
    -- The value of the specified part of the web request must exactly match
    -- the value of @SearchString@.
    --
    -- __STARTS_WITH__
    --
    -- The value of @SearchString@ must appear at the beginning of the
    -- specified part of the web request.
    --
    -- __ENDS_WITH__
    --
    -- The value of @SearchString@ must appear at the end of the specified part
    -- of the web request.
    positionalConstraint :: PositionalConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ByteMatchStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'searchString', 'byteMatchStatement_searchString' - A string value that you want WAF to search for. WAF searches only in the
-- part of web requests that you designate for inspection in FieldToMatch.
-- The maximum length of the value is 50 bytes.
--
-- Valid values depend on the component that you specify for inspection in
-- @FieldToMatch@:
--
-- -   @Method@: The HTTP method that you want WAF to search for. This
--     indicates the type of operation specified in the request.
--
-- -   @UriPath@: The value that you want WAF to search for in the URI
--     path, for example, @\/images\/daily-ad.jpg@.
--
-- If @SearchString@ includes alphabetic characters A-Z and a-z, note that
-- the value is case sensitive.
--
-- __If you\'re using the WAF API__
--
-- Specify a base64-encoded version of the value. The maximum length of the
-- value before you base64-encode it is 50 bytes.
--
-- For example, suppose the value of @Type@ is @HEADER@ and the value of
-- @Data@ is @User-Agent@. If you want to search the @User-Agent@ header
-- for the value @BadBot@, you base64-encode @BadBot@ using MIME
-- base64-encoding and include the resulting value, @QmFkQm90@, in the
-- value of @SearchString@.
--
-- __If you\'re using the CLI or one of the Amazon Web Services SDKs__
--
-- The value that you want WAF to search for. The SDK automatically base64
-- encodes the value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'fieldToMatch', 'byteMatchStatement_fieldToMatch' - The part of the web request that you want WAF to inspect.
--
-- 'textTransformations', 'byteMatchStatement_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
--
-- 'positionalConstraint', 'byteMatchStatement_positionalConstraint' - The area within the portion of the web request that you want WAF to
-- search for @SearchString@. Valid values include the following:
--
-- __CONTAINS__
--
-- The specified part of the web request must include the value of
-- @SearchString@, but the location doesn\'t matter.
--
-- __CONTAINS_WORD__
--
-- The specified part of the web request must include the value of
-- @SearchString@, and @SearchString@ must contain only alphanumeric
-- characters or underscore (A-Z, a-z, 0-9, or _). In addition,
-- @SearchString@ must be a word, which means that both of the following
-- are true:
--
-- -   @SearchString@ is at the beginning of the specified part of the web
--     request or is preceded by a character other than an alphanumeric
--     character or underscore (_). Examples include the value of a header
--     and @;BadBot@.
--
-- -   @SearchString@ is at the end of the specified part of the web
--     request or is followed by a character other than an alphanumeric
--     character or underscore (_), for example, @BadBot;@ and @-BadBot;@.
--
-- __EXACTLY__
--
-- The value of the specified part of the web request must exactly match
-- the value of @SearchString@.
--
-- __STARTS_WITH__
--
-- The value of @SearchString@ must appear at the beginning of the
-- specified part of the web request.
--
-- __ENDS_WITH__
--
-- The value of @SearchString@ must appear at the end of the specified part
-- of the web request.
newByteMatchStatement ::
  -- | 'searchString'
  Prelude.ByteString ->
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  -- | 'positionalConstraint'
  PositionalConstraint ->
  ByteMatchStatement
newByteMatchStatement
  pSearchString_
  pFieldToMatch_
  pTextTransformations_
  pPositionalConstraint_ =
    ByteMatchStatement'
      { searchString =
          Data._Base64 Lens.# pSearchString_,
        fieldToMatch = pFieldToMatch_,
        textTransformations =
          Lens.coerced Lens.# pTextTransformations_,
        positionalConstraint = pPositionalConstraint_
      }

-- | A string value that you want WAF to search for. WAF searches only in the
-- part of web requests that you designate for inspection in FieldToMatch.
-- The maximum length of the value is 50 bytes.
--
-- Valid values depend on the component that you specify for inspection in
-- @FieldToMatch@:
--
-- -   @Method@: The HTTP method that you want WAF to search for. This
--     indicates the type of operation specified in the request.
--
-- -   @UriPath@: The value that you want WAF to search for in the URI
--     path, for example, @\/images\/daily-ad.jpg@.
--
-- If @SearchString@ includes alphabetic characters A-Z and a-z, note that
-- the value is case sensitive.
--
-- __If you\'re using the WAF API__
--
-- Specify a base64-encoded version of the value. The maximum length of the
-- value before you base64-encode it is 50 bytes.
--
-- For example, suppose the value of @Type@ is @HEADER@ and the value of
-- @Data@ is @User-Agent@. If you want to search the @User-Agent@ header
-- for the value @BadBot@, you base64-encode @BadBot@ using MIME
-- base64-encoding and include the resulting value, @QmFkQm90@, in the
-- value of @SearchString@.
--
-- __If you\'re using the CLI or one of the Amazon Web Services SDKs__
--
-- The value that you want WAF to search for. The SDK automatically base64
-- encodes the value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
byteMatchStatement_searchString :: Lens.Lens' ByteMatchStatement Prelude.ByteString
byteMatchStatement_searchString = Lens.lens (\ByteMatchStatement' {searchString} -> searchString) (\s@ByteMatchStatement' {} a -> s {searchString = a} :: ByteMatchStatement) Prelude.. Data._Base64

-- | The part of the web request that you want WAF to inspect.
byteMatchStatement_fieldToMatch :: Lens.Lens' ByteMatchStatement FieldToMatch
byteMatchStatement_fieldToMatch = Lens.lens (\ByteMatchStatement' {fieldToMatch} -> fieldToMatch) (\s@ByteMatchStatement' {} a -> s {fieldToMatch = a} :: ByteMatchStatement)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
byteMatchStatement_textTransformations :: Lens.Lens' ByteMatchStatement (Prelude.NonEmpty TextTransformation)
byteMatchStatement_textTransformations = Lens.lens (\ByteMatchStatement' {textTransformations} -> textTransformations) (\s@ByteMatchStatement' {} a -> s {textTransformations = a} :: ByteMatchStatement) Prelude.. Lens.coerced

-- | The area within the portion of the web request that you want WAF to
-- search for @SearchString@. Valid values include the following:
--
-- __CONTAINS__
--
-- The specified part of the web request must include the value of
-- @SearchString@, but the location doesn\'t matter.
--
-- __CONTAINS_WORD__
--
-- The specified part of the web request must include the value of
-- @SearchString@, and @SearchString@ must contain only alphanumeric
-- characters or underscore (A-Z, a-z, 0-9, or _). In addition,
-- @SearchString@ must be a word, which means that both of the following
-- are true:
--
-- -   @SearchString@ is at the beginning of the specified part of the web
--     request or is preceded by a character other than an alphanumeric
--     character or underscore (_). Examples include the value of a header
--     and @;BadBot@.
--
-- -   @SearchString@ is at the end of the specified part of the web
--     request or is followed by a character other than an alphanumeric
--     character or underscore (_), for example, @BadBot;@ and @-BadBot;@.
--
-- __EXACTLY__
--
-- The value of the specified part of the web request must exactly match
-- the value of @SearchString@.
--
-- __STARTS_WITH__
--
-- The value of @SearchString@ must appear at the beginning of the
-- specified part of the web request.
--
-- __ENDS_WITH__
--
-- The value of @SearchString@ must appear at the end of the specified part
-- of the web request.
byteMatchStatement_positionalConstraint :: Lens.Lens' ByteMatchStatement PositionalConstraint
byteMatchStatement_positionalConstraint = Lens.lens (\ByteMatchStatement' {positionalConstraint} -> positionalConstraint) (\s@ByteMatchStatement' {} a -> s {positionalConstraint = a} :: ByteMatchStatement)

instance Data.FromJSON ByteMatchStatement where
  parseJSON =
    Data.withObject
      "ByteMatchStatement"
      ( \x ->
          ByteMatchStatement'
            Prelude.<$> (x Data..: "SearchString")
            Prelude.<*> (x Data..: "FieldToMatch")
            Prelude.<*> (x Data..: "TextTransformations")
            Prelude.<*> (x Data..: "PositionalConstraint")
      )

instance Prelude.Hashable ByteMatchStatement where
  hashWithSalt _salt ByteMatchStatement' {..} =
    _salt
      `Prelude.hashWithSalt` searchString
      `Prelude.hashWithSalt` fieldToMatch
      `Prelude.hashWithSalt` textTransformations
      `Prelude.hashWithSalt` positionalConstraint

instance Prelude.NFData ByteMatchStatement where
  rnf ByteMatchStatement' {..} =
    Prelude.rnf searchString
      `Prelude.seq` Prelude.rnf fieldToMatch
      `Prelude.seq` Prelude.rnf textTransformations
      `Prelude.seq` Prelude.rnf positionalConstraint

instance Data.ToJSON ByteMatchStatement where
  toJSON ByteMatchStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SearchString" Data..= searchString),
            Prelude.Just ("FieldToMatch" Data..= fieldToMatch),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations),
            Prelude.Just
              ( "PositionalConstraint"
                  Data..= positionalConstraint
              )
          ]
      )
