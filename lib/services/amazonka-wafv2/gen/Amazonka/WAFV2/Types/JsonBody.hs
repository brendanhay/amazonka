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
-- Module      : Amazonka.WAFV2.Types.JsonBody
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.JsonBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.BodyParsingFallbackBehavior
import Amazonka.WAFV2.Types.JsonMatchPattern
import Amazonka.WAFV2.Types.JsonMatchScope
import Amazonka.WAFV2.Types.OversizeHandling

-- | Inspect the body of the web request as JSON. The body immediately
-- follows the request headers.
--
-- This is used to indicate the web request component to inspect, in the
-- FieldToMatch specification.
--
-- Use the specifications in this object to indicate which parts of the
-- JSON body to inspect using the rule\'s inspection criteria. WAF inspects
-- only the parts of the JSON that result from the matches that you
-- indicate.
--
-- Example JSON:
-- @\"JsonBody\": { \"MatchPattern\": { \"All\": {} }, \"MatchScope\": \"ALL\" }@
--
-- /See:/ 'newJsonBody' smart constructor.
data JsonBody = JsonBody'
  { -- | What WAF should do if it fails to completely parse the JSON body. The
    -- options are the following:
    --
    -- -   @EVALUATE_AS_STRING@ - Inspect the body as plain text. WAF applies
    --     the text transformations and inspection criteria that you defined
    --     for the JSON inspection to the body text string.
    --
    -- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
    --     applies the rule action to the request.
    --
    -- -   @NO_MATCH@ - Treat the web request as not matching the rule
    --     statement.
    --
    -- If you don\'t provide this setting, WAF parses and evaluates the content
    -- only up to the first parsing failure that it encounters.
    --
    -- WAF does its best to parse the entire JSON body, but might be forced to
    -- stop for reasons such as invalid characters, duplicate keys, truncation,
    -- and any content whose root node isn\'t an object or an array.
    --
    -- WAF parses the JSON in the following examples as two valid key, value
    -- pairs:
    --
    -- -   Missing comma: @{\"key1\":\"value1\"\"key2\":\"value2\"}@
    --
    -- -   Missing colon: @{\"key1\":\"value1\",\"key2\"\"value2\"}@
    --
    -- -   Extra colons: @{\"key1\"::\"value1\",\"key2\"\"value2\"}@
    invalidFallbackBehavior :: Prelude.Maybe BodyParsingFallbackBehavior,
    -- | What WAF should do if the body is larger than WAF can inspect. WAF does
    -- not support inspecting the entire contents of the body of a web request
    -- when the body exceeds 8 KB (8192 bytes). Only the first 8 KB of the
    -- request body are forwarded to WAF by the underlying host service.
    --
    -- The options for oversize handling are the following:
    --
    -- -   @CONTINUE@ - Inspect the body normally, according to the rule
    --     inspection criteria.
    --
    -- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
    --     applies the rule action to the request.
    --
    -- -   @NO_MATCH@ - Treat the web request as not matching the rule
    --     statement.
    --
    -- You can combine the @MATCH@ or @NO_MATCH@ settings for oversize handling
    -- with your rule and web ACL action settings, so that you block any
    -- request whose body is over 8 KB.
    --
    -- Default: @CONTINUE@
    oversizeHandling :: Prelude.Maybe OversizeHandling,
    -- | The patterns to look for in the JSON body. WAF inspects the results of
    -- these pattern matches against the rule inspection criteria.
    matchPattern :: JsonMatchPattern,
    -- | The parts of the JSON to match against using the @MatchPattern@. If you
    -- specify @All@, WAF matches against keys and values.
    matchScope :: JsonMatchScope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JsonBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidFallbackBehavior', 'jsonBody_invalidFallbackBehavior' - What WAF should do if it fails to completely parse the JSON body. The
-- options are the following:
--
-- -   @EVALUATE_AS_STRING@ - Inspect the body as plain text. WAF applies
--     the text transformations and inspection criteria that you defined
--     for the JSON inspection to the body text string.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- If you don\'t provide this setting, WAF parses and evaluates the content
-- only up to the first parsing failure that it encounters.
--
-- WAF does its best to parse the entire JSON body, but might be forced to
-- stop for reasons such as invalid characters, duplicate keys, truncation,
-- and any content whose root node isn\'t an object or an array.
--
-- WAF parses the JSON in the following examples as two valid key, value
-- pairs:
--
-- -   Missing comma: @{\"key1\":\"value1\"\"key2\":\"value2\"}@
--
-- -   Missing colon: @{\"key1\":\"value1\",\"key2\"\"value2\"}@
--
-- -   Extra colons: @{\"key1\"::\"value1\",\"key2\"\"value2\"}@
--
-- 'oversizeHandling', 'jsonBody_oversizeHandling' - What WAF should do if the body is larger than WAF can inspect. WAF does
-- not support inspecting the entire contents of the body of a web request
-- when the body exceeds 8 KB (8192 bytes). Only the first 8 KB of the
-- request body are forwarded to WAF by the underlying host service.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the body normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- You can combine the @MATCH@ or @NO_MATCH@ settings for oversize handling
-- with your rule and web ACL action settings, so that you block any
-- request whose body is over 8 KB.
--
-- Default: @CONTINUE@
--
-- 'matchPattern', 'jsonBody_matchPattern' - The patterns to look for in the JSON body. WAF inspects the results of
-- these pattern matches against the rule inspection criteria.
--
-- 'matchScope', 'jsonBody_matchScope' - The parts of the JSON to match against using the @MatchPattern@. If you
-- specify @All@, WAF matches against keys and values.
newJsonBody ::
  -- | 'matchPattern'
  JsonMatchPattern ->
  -- | 'matchScope'
  JsonMatchScope ->
  JsonBody
newJsonBody pMatchPattern_ pMatchScope_ =
  JsonBody'
    { invalidFallbackBehavior =
        Prelude.Nothing,
      oversizeHandling = Prelude.Nothing,
      matchPattern = pMatchPattern_,
      matchScope = pMatchScope_
    }

-- | What WAF should do if it fails to completely parse the JSON body. The
-- options are the following:
--
-- -   @EVALUATE_AS_STRING@ - Inspect the body as plain text. WAF applies
--     the text transformations and inspection criteria that you defined
--     for the JSON inspection to the body text string.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- If you don\'t provide this setting, WAF parses and evaluates the content
-- only up to the first parsing failure that it encounters.
--
-- WAF does its best to parse the entire JSON body, but might be forced to
-- stop for reasons such as invalid characters, duplicate keys, truncation,
-- and any content whose root node isn\'t an object or an array.
--
-- WAF parses the JSON in the following examples as two valid key, value
-- pairs:
--
-- -   Missing comma: @{\"key1\":\"value1\"\"key2\":\"value2\"}@
--
-- -   Missing colon: @{\"key1\":\"value1\",\"key2\"\"value2\"}@
--
-- -   Extra colons: @{\"key1\"::\"value1\",\"key2\"\"value2\"}@
jsonBody_invalidFallbackBehavior :: Lens.Lens' JsonBody (Prelude.Maybe BodyParsingFallbackBehavior)
jsonBody_invalidFallbackBehavior = Lens.lens (\JsonBody' {invalidFallbackBehavior} -> invalidFallbackBehavior) (\s@JsonBody' {} a -> s {invalidFallbackBehavior = a} :: JsonBody)

-- | What WAF should do if the body is larger than WAF can inspect. WAF does
-- not support inspecting the entire contents of the body of a web request
-- when the body exceeds 8 KB (8192 bytes). Only the first 8 KB of the
-- request body are forwarded to WAF by the underlying host service.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the body normally, according to the rule
--     inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
--
-- You can combine the @MATCH@ or @NO_MATCH@ settings for oversize handling
-- with your rule and web ACL action settings, so that you block any
-- request whose body is over 8 KB.
--
-- Default: @CONTINUE@
jsonBody_oversizeHandling :: Lens.Lens' JsonBody (Prelude.Maybe OversizeHandling)
jsonBody_oversizeHandling = Lens.lens (\JsonBody' {oversizeHandling} -> oversizeHandling) (\s@JsonBody' {} a -> s {oversizeHandling = a} :: JsonBody)

-- | The patterns to look for in the JSON body. WAF inspects the results of
-- these pattern matches against the rule inspection criteria.
jsonBody_matchPattern :: Lens.Lens' JsonBody JsonMatchPattern
jsonBody_matchPattern = Lens.lens (\JsonBody' {matchPattern} -> matchPattern) (\s@JsonBody' {} a -> s {matchPattern = a} :: JsonBody)

-- | The parts of the JSON to match against using the @MatchPattern@. If you
-- specify @All@, WAF matches against keys and values.
jsonBody_matchScope :: Lens.Lens' JsonBody JsonMatchScope
jsonBody_matchScope = Lens.lens (\JsonBody' {matchScope} -> matchScope) (\s@JsonBody' {} a -> s {matchScope = a} :: JsonBody)

instance Data.FromJSON JsonBody where
  parseJSON =
    Data.withObject
      "JsonBody"
      ( \x ->
          JsonBody'
            Prelude.<$> (x Data..:? "InvalidFallbackBehavior")
            Prelude.<*> (x Data..:? "OversizeHandling")
            Prelude.<*> (x Data..: "MatchPattern")
            Prelude.<*> (x Data..: "MatchScope")
      )

instance Prelude.Hashable JsonBody where
  hashWithSalt _salt JsonBody' {..} =
    _salt
      `Prelude.hashWithSalt` invalidFallbackBehavior
      `Prelude.hashWithSalt` oversizeHandling
      `Prelude.hashWithSalt` matchPattern
      `Prelude.hashWithSalt` matchScope

instance Prelude.NFData JsonBody where
  rnf JsonBody' {..} =
    Prelude.rnf invalidFallbackBehavior
      `Prelude.seq` Prelude.rnf oversizeHandling
      `Prelude.seq` Prelude.rnf matchPattern
      `Prelude.seq` Prelude.rnf matchScope

instance Data.ToJSON JsonBody where
  toJSON JsonBody' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvalidFallbackBehavior" Data..=)
              Prelude.<$> invalidFallbackBehavior,
            ("OversizeHandling" Data..=)
              Prelude.<$> oversizeHandling,
            Prelude.Just ("MatchPattern" Data..= matchPattern),
            Prelude.Just ("MatchScope" Data..= matchScope)
          ]
      )
