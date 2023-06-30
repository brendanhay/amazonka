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
-- Module      : Amazonka.WAFV2.Types.TextTransformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.TextTransformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.TextTransformationType

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection.
--
-- /See:/ 'newTextTransformation' smart constructor.
data TextTransformation = TextTransformation'
  { -- | Sets the relative processing order for multiple transformations that are
    -- defined for a rule statement. WAF processes all transformations, from
    -- lowest priority to highest, before inspecting the transformed content.
    -- The priorities don\'t need to be consecutive, but they must all be
    -- different.
    priority :: Prelude.Natural,
    -- | You can specify the following transformation types:
    --
    -- __BASE64_DECODE__ - Decode a @Base64@-encoded string.
    --
    -- __BASE64_DECODE_EXT__ - Decode a @Base64@-encoded string, but use a
    -- forgiving implementation that ignores characters that aren\'t valid.
    --
    -- __CMD_LINE__ - Command-line transformations. These are helpful in
    -- reducing effectiveness of attackers who inject an operating system
    -- command-line command and use unusual formatting to disguise some or all
    -- of the command.
    --
    -- -   Delete the following characters: @\\ \" \' ^@
    --
    -- -   Delete spaces before the following characters: @\/ (@
    --
    -- -   Replace the following characters with a space: @, ;@
    --
    -- -   Replace multiple spaces with one space
    --
    -- -   Convert uppercase letters (A-Z) to lowercase (a-z)
    --
    -- __COMPRESS_WHITE_SPACE__ - Replace these characters with a space
    -- character (decimal 32):
    --
    -- -   @\\f@, formfeed, decimal 12
    --
    -- -   @\\t@, tab, decimal 9
    --
    -- -   @\\n@, newline, decimal 10
    --
    -- -   @\\r@, carriage return, decimal 13
    --
    -- -   @\\v@, vertical tab, decimal 11
    --
    -- -   Non-breaking space, decimal 160
    --
    -- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
    --
    -- __CSS_DECODE__ - Decode characters that were encoded using CSS 2.x
    -- escape rules @syndata.html#characters@. This function uses up to two
    -- bytes in the decoding process, so it can help to uncover ASCII
    -- characters that were encoded using CSS encoding that wouldn’t typically
    -- be encoded. It\'s also useful in countering evasion, which is a
    -- combination of a backslash and non-hexadecimal characters. For example,
    -- @ja\\vascript@ for javascript.
    --
    -- __ESCAPE_SEQ_DECODE__ - Decode the following ANSI C escape sequences:
    -- @\\a@, @\\b@, @\\f@, @\\n@, @\\r@, @\\t@, @\\v@, @\\\\@, @\\?@, @\\\'@,
    -- @\\\"@, @\\xHH@ (hexadecimal), @\\0OOO@ (octal). Encodings that aren\'t
    -- valid remain in the output.
    --
    -- __HEX_DECODE__ - Decode a string of hexadecimal characters into a
    -- binary.
    --
    -- __HTML_ENTITY_DECODE__ - Replace HTML-encoded characters with unencoded
    -- characters. @HTML_ENTITY_DECODE@ performs these operations:
    --
    -- -   Replaces @(ampersand)quot;@ with @\"@
    --
    -- -   Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
    --
    -- -   Replaces @(ampersand)lt;@ with a \"less than\" symbol
    --
    -- -   Replaces @(ampersand)gt;@ with @>@
    --
    -- -   Replaces characters that are represented in hexadecimal format,
    --     @(ampersand)#xhhhh;@, with the corresponding characters
    --
    -- -   Replaces characters that are represented in decimal format,
    --     @(ampersand)#nnnn;@, with the corresponding characters
    --
    -- __JS_DECODE__ - Decode JavaScript escape sequences. If a @\\@ @u@ @HHHH@
    -- code is in the full-width ASCII code range of @FF01-FF5E@, then the
    -- higher byte is used to detect and adjust the lower byte. If not, only
    -- the lower byte is used and the higher byte is zeroed, causing a possible
    -- loss of information.
    --
    -- __LOWERCASE__ - Convert uppercase letters (A-Z) to lowercase (a-z).
    --
    -- __MD5__ - Calculate an MD5 hash from the data in the input. The computed
    -- hash is in a raw binary form.
    --
    -- __NONE__ - Specify @NONE@ if you don\'t want any text transformations.
    --
    -- __NORMALIZE_PATH__ - Remove multiple slashes, directory self-references,
    -- and directory back-references that are not at the beginning of the input
    -- from an input string.
    --
    -- __NORMALIZE_PATH_WIN__ - This is the same as @NORMALIZE_PATH@, but first
    -- converts backslash characters to forward slashes.
    --
    -- __REMOVE_NULLS__ - Remove all @NULL@ bytes from the input.
    --
    -- __REPLACE_COMMENTS__ - Replace each occurrence of a C-style comment
    -- (@\/* ... *\/@) with a single space. Multiple consecutive occurrences
    -- are not compressed. Unterminated comments are also replaced with a space
    -- (ASCII 0x20). However, a standalone termination of a comment (@*\/@) is
    -- not acted upon.
    --
    -- __REPLACE_NULLS__ - Replace NULL bytes in the input with space
    -- characters (ASCII @0x20@).
    --
    -- __SQL_HEX_DECODE__ - Decode SQL hex data. Example (@0x414243@) will be
    -- decoded to (@ABC@).
    --
    -- __URL_DECODE__ - Decode a URL-encoded value.
    --
    -- __URL_DECODE_UNI__ - Like @URL_DECODE@, but with support for
    -- Microsoft-specific @%u@ encoding. If the code is in the full-width ASCII
    -- code range of @FF01-FF5E@, the higher byte is used to detect and adjust
    -- the lower byte. Otherwise, only the lower byte is used and the higher
    -- byte is zeroed.
    --
    -- __UTF8_TO_UNICODE__ - Convert all UTF-8 character sequences to Unicode.
    -- This helps input normalization, and minimizing false-positives and
    -- false-negatives for non-English languages.
    type' :: TextTransformationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextTransformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'textTransformation_priority' - Sets the relative processing order for multiple transformations that are
-- defined for a rule statement. WAF processes all transformations, from
-- lowest priority to highest, before inspecting the transformed content.
-- The priorities don\'t need to be consecutive, but they must all be
-- different.
--
-- 'type'', 'textTransformation_type' - You can specify the following transformation types:
--
-- __BASE64_DECODE__ - Decode a @Base64@-encoded string.
--
-- __BASE64_DECODE_EXT__ - Decode a @Base64@-encoded string, but use a
-- forgiving implementation that ignores characters that aren\'t valid.
--
-- __CMD_LINE__ - Command-line transformations. These are helpful in
-- reducing effectiveness of attackers who inject an operating system
-- command-line command and use unusual formatting to disguise some or all
-- of the command.
--
-- -   Delete the following characters: @\\ \" \' ^@
--
-- -   Delete spaces before the following characters: @\/ (@
--
-- -   Replace the following characters with a space: @, ;@
--
-- -   Replace multiple spaces with one space
--
-- -   Convert uppercase letters (A-Z) to lowercase (a-z)
--
-- __COMPRESS_WHITE_SPACE__ - Replace these characters with a space
-- character (decimal 32):
--
-- -   @\\f@, formfeed, decimal 12
--
-- -   @\\t@, tab, decimal 9
--
-- -   @\\n@, newline, decimal 10
--
-- -   @\\r@, carriage return, decimal 13
--
-- -   @\\v@, vertical tab, decimal 11
--
-- -   Non-breaking space, decimal 160
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
--
-- __CSS_DECODE__ - Decode characters that were encoded using CSS 2.x
-- escape rules @syndata.html#characters@. This function uses up to two
-- bytes in the decoding process, so it can help to uncover ASCII
-- characters that were encoded using CSS encoding that wouldn’t typically
-- be encoded. It\'s also useful in countering evasion, which is a
-- combination of a backslash and non-hexadecimal characters. For example,
-- @ja\\vascript@ for javascript.
--
-- __ESCAPE_SEQ_DECODE__ - Decode the following ANSI C escape sequences:
-- @\\a@, @\\b@, @\\f@, @\\n@, @\\r@, @\\t@, @\\v@, @\\\\@, @\\?@, @\\\'@,
-- @\\\"@, @\\xHH@ (hexadecimal), @\\0OOO@ (octal). Encodings that aren\'t
-- valid remain in the output.
--
-- __HEX_DECODE__ - Decode a string of hexadecimal characters into a
-- binary.
--
-- __HTML_ENTITY_DECODE__ - Replace HTML-encoded characters with unencoded
-- characters. @HTML_ENTITY_DECODE@ performs these operations:
--
-- -   Replaces @(ampersand)quot;@ with @\"@
--
-- -   Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
-- -   Replaces @(ampersand)lt;@ with a \"less than\" symbol
--
-- -   Replaces @(ampersand)gt;@ with @>@
--
-- -   Replaces characters that are represented in hexadecimal format,
--     @(ampersand)#xhhhh;@, with the corresponding characters
--
-- -   Replaces characters that are represented in decimal format,
--     @(ampersand)#nnnn;@, with the corresponding characters
--
-- __JS_DECODE__ - Decode JavaScript escape sequences. If a @\\@ @u@ @HHHH@
-- code is in the full-width ASCII code range of @FF01-FF5E@, then the
-- higher byte is used to detect and adjust the lower byte. If not, only
-- the lower byte is used and the higher byte is zeroed, causing a possible
-- loss of information.
--
-- __LOWERCASE__ - Convert uppercase letters (A-Z) to lowercase (a-z).
--
-- __MD5__ - Calculate an MD5 hash from the data in the input. The computed
-- hash is in a raw binary form.
--
-- __NONE__ - Specify @NONE@ if you don\'t want any text transformations.
--
-- __NORMALIZE_PATH__ - Remove multiple slashes, directory self-references,
-- and directory back-references that are not at the beginning of the input
-- from an input string.
--
-- __NORMALIZE_PATH_WIN__ - This is the same as @NORMALIZE_PATH@, but first
-- converts backslash characters to forward slashes.
--
-- __REMOVE_NULLS__ - Remove all @NULL@ bytes from the input.
--
-- __REPLACE_COMMENTS__ - Replace each occurrence of a C-style comment
-- (@\/* ... *\/@) with a single space. Multiple consecutive occurrences
-- are not compressed. Unterminated comments are also replaced with a space
-- (ASCII 0x20). However, a standalone termination of a comment (@*\/@) is
-- not acted upon.
--
-- __REPLACE_NULLS__ - Replace NULL bytes in the input with space
-- characters (ASCII @0x20@).
--
-- __SQL_HEX_DECODE__ - Decode SQL hex data. Example (@0x414243@) will be
-- decoded to (@ABC@).
--
-- __URL_DECODE__ - Decode a URL-encoded value.
--
-- __URL_DECODE_UNI__ - Like @URL_DECODE@, but with support for
-- Microsoft-specific @%u@ encoding. If the code is in the full-width ASCII
-- code range of @FF01-FF5E@, the higher byte is used to detect and adjust
-- the lower byte. Otherwise, only the lower byte is used and the higher
-- byte is zeroed.
--
-- __UTF8_TO_UNICODE__ - Convert all UTF-8 character sequences to Unicode.
-- This helps input normalization, and minimizing false-positives and
-- false-negatives for non-English languages.
newTextTransformation ::
  -- | 'priority'
  Prelude.Natural ->
  -- | 'type''
  TextTransformationType ->
  TextTransformation
newTextTransformation pPriority_ pType_ =
  TextTransformation'
    { priority = pPriority_,
      type' = pType_
    }

-- | Sets the relative processing order for multiple transformations that are
-- defined for a rule statement. WAF processes all transformations, from
-- lowest priority to highest, before inspecting the transformed content.
-- The priorities don\'t need to be consecutive, but they must all be
-- different.
textTransformation_priority :: Lens.Lens' TextTransformation Prelude.Natural
textTransformation_priority = Lens.lens (\TextTransformation' {priority} -> priority) (\s@TextTransformation' {} a -> s {priority = a} :: TextTransformation)

-- | You can specify the following transformation types:
--
-- __BASE64_DECODE__ - Decode a @Base64@-encoded string.
--
-- __BASE64_DECODE_EXT__ - Decode a @Base64@-encoded string, but use a
-- forgiving implementation that ignores characters that aren\'t valid.
--
-- __CMD_LINE__ - Command-line transformations. These are helpful in
-- reducing effectiveness of attackers who inject an operating system
-- command-line command and use unusual formatting to disguise some or all
-- of the command.
--
-- -   Delete the following characters: @\\ \" \' ^@
--
-- -   Delete spaces before the following characters: @\/ (@
--
-- -   Replace the following characters with a space: @, ;@
--
-- -   Replace multiple spaces with one space
--
-- -   Convert uppercase letters (A-Z) to lowercase (a-z)
--
-- __COMPRESS_WHITE_SPACE__ - Replace these characters with a space
-- character (decimal 32):
--
-- -   @\\f@, formfeed, decimal 12
--
-- -   @\\t@, tab, decimal 9
--
-- -   @\\n@, newline, decimal 10
--
-- -   @\\r@, carriage return, decimal 13
--
-- -   @\\v@, vertical tab, decimal 11
--
-- -   Non-breaking space, decimal 160
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
--
-- __CSS_DECODE__ - Decode characters that were encoded using CSS 2.x
-- escape rules @syndata.html#characters@. This function uses up to two
-- bytes in the decoding process, so it can help to uncover ASCII
-- characters that were encoded using CSS encoding that wouldn’t typically
-- be encoded. It\'s also useful in countering evasion, which is a
-- combination of a backslash and non-hexadecimal characters. For example,
-- @ja\\vascript@ for javascript.
--
-- __ESCAPE_SEQ_DECODE__ - Decode the following ANSI C escape sequences:
-- @\\a@, @\\b@, @\\f@, @\\n@, @\\r@, @\\t@, @\\v@, @\\\\@, @\\?@, @\\\'@,
-- @\\\"@, @\\xHH@ (hexadecimal), @\\0OOO@ (octal). Encodings that aren\'t
-- valid remain in the output.
--
-- __HEX_DECODE__ - Decode a string of hexadecimal characters into a
-- binary.
--
-- __HTML_ENTITY_DECODE__ - Replace HTML-encoded characters with unencoded
-- characters. @HTML_ENTITY_DECODE@ performs these operations:
--
-- -   Replaces @(ampersand)quot;@ with @\"@
--
-- -   Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
-- -   Replaces @(ampersand)lt;@ with a \"less than\" symbol
--
-- -   Replaces @(ampersand)gt;@ with @>@
--
-- -   Replaces characters that are represented in hexadecimal format,
--     @(ampersand)#xhhhh;@, with the corresponding characters
--
-- -   Replaces characters that are represented in decimal format,
--     @(ampersand)#nnnn;@, with the corresponding characters
--
-- __JS_DECODE__ - Decode JavaScript escape sequences. If a @\\@ @u@ @HHHH@
-- code is in the full-width ASCII code range of @FF01-FF5E@, then the
-- higher byte is used to detect and adjust the lower byte. If not, only
-- the lower byte is used and the higher byte is zeroed, causing a possible
-- loss of information.
--
-- __LOWERCASE__ - Convert uppercase letters (A-Z) to lowercase (a-z).
--
-- __MD5__ - Calculate an MD5 hash from the data in the input. The computed
-- hash is in a raw binary form.
--
-- __NONE__ - Specify @NONE@ if you don\'t want any text transformations.
--
-- __NORMALIZE_PATH__ - Remove multiple slashes, directory self-references,
-- and directory back-references that are not at the beginning of the input
-- from an input string.
--
-- __NORMALIZE_PATH_WIN__ - This is the same as @NORMALIZE_PATH@, but first
-- converts backslash characters to forward slashes.
--
-- __REMOVE_NULLS__ - Remove all @NULL@ bytes from the input.
--
-- __REPLACE_COMMENTS__ - Replace each occurrence of a C-style comment
-- (@\/* ... *\/@) with a single space. Multiple consecutive occurrences
-- are not compressed. Unterminated comments are also replaced with a space
-- (ASCII 0x20). However, a standalone termination of a comment (@*\/@) is
-- not acted upon.
--
-- __REPLACE_NULLS__ - Replace NULL bytes in the input with space
-- characters (ASCII @0x20@).
--
-- __SQL_HEX_DECODE__ - Decode SQL hex data. Example (@0x414243@) will be
-- decoded to (@ABC@).
--
-- __URL_DECODE__ - Decode a URL-encoded value.
--
-- __URL_DECODE_UNI__ - Like @URL_DECODE@, but with support for
-- Microsoft-specific @%u@ encoding. If the code is in the full-width ASCII
-- code range of @FF01-FF5E@, the higher byte is used to detect and adjust
-- the lower byte. Otherwise, only the lower byte is used and the higher
-- byte is zeroed.
--
-- __UTF8_TO_UNICODE__ - Convert all UTF-8 character sequences to Unicode.
-- This helps input normalization, and minimizing false-positives and
-- false-negatives for non-English languages.
textTransformation_type :: Lens.Lens' TextTransformation TextTransformationType
textTransformation_type = Lens.lens (\TextTransformation' {type'} -> type') (\s@TextTransformation' {} a -> s {type' = a} :: TextTransformation)

instance Data.FromJSON TextTransformation where
  parseJSON =
    Data.withObject
      "TextTransformation"
      ( \x ->
          TextTransformation'
            Prelude.<$> (x Data..: "Priority")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable TextTransformation where
  hashWithSalt _salt TextTransformation' {..} =
    _salt
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TextTransformation where
  rnf TextTransformation' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON TextTransformation where
  toJSON TextTransformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Priority" Data..= priority),
            Prelude.Just ("Type" Data..= type')
          ]
      )
