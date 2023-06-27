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
-- Module      : Amazonka.WAFV2.Types.ResponseInspectionBodyContains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResponseInspectionBodyContains where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures inspection of the response body. WAF can inspect the first
-- 65,536 bytes (64 KB) of the response body. This is part of the
-- @ResponseInspection@ configuration for @AWSManagedRulesATPRuleSet@ and
-- @AWSManagedRulesACFPRuleSet@.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- /See:/ 'newResponseInspectionBodyContains' smart constructor.
data ResponseInspectionBodyContains = ResponseInspectionBodyContains'
  { -- | Strings in the body of the response that indicate a successful login or
    -- account creation attempt. To be counted as a success, the string can be
    -- anywhere in the body and must be an exact match, including case. Each
    -- string must be unique among the success and failure strings.
    --
    -- JSON examples: @\"SuccessStrings\": [ \"Login successful\" ]@ and
    -- @\"SuccessStrings\": [ \"Account creation successful\", \"Welcome to our site!\" ]@
    successStrings :: Prelude.NonEmpty Prelude.Text,
    -- | Strings in the body of the response that indicate a failed login or
    -- account creation attempt. To be counted as a failure, the string can be
    -- anywhere in the body and must be an exact match, including case. Each
    -- string must be unique among the success and failure strings.
    --
    -- JSON example: @\"FailureStrings\": [ \"Request failed\" ]@
    failureStrings :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseInspectionBodyContains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successStrings', 'responseInspectionBodyContains_successStrings' - Strings in the body of the response that indicate a successful login or
-- account creation attempt. To be counted as a success, the string can be
-- anywhere in the body and must be an exact match, including case. Each
-- string must be unique among the success and failure strings.
--
-- JSON examples: @\"SuccessStrings\": [ \"Login successful\" ]@ and
-- @\"SuccessStrings\": [ \"Account creation successful\", \"Welcome to our site!\" ]@
--
-- 'failureStrings', 'responseInspectionBodyContains_failureStrings' - Strings in the body of the response that indicate a failed login or
-- account creation attempt. To be counted as a failure, the string can be
-- anywhere in the body and must be an exact match, including case. Each
-- string must be unique among the success and failure strings.
--
-- JSON example: @\"FailureStrings\": [ \"Request failed\" ]@
newResponseInspectionBodyContains ::
  -- | 'successStrings'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'failureStrings'
  Prelude.NonEmpty Prelude.Text ->
  ResponseInspectionBodyContains
newResponseInspectionBodyContains
  pSuccessStrings_
  pFailureStrings_ =
    ResponseInspectionBodyContains'
      { successStrings =
          Lens.coerced Lens.# pSuccessStrings_,
        failureStrings =
          Lens.coerced Lens.# pFailureStrings_
      }

-- | Strings in the body of the response that indicate a successful login or
-- account creation attempt. To be counted as a success, the string can be
-- anywhere in the body and must be an exact match, including case. Each
-- string must be unique among the success and failure strings.
--
-- JSON examples: @\"SuccessStrings\": [ \"Login successful\" ]@ and
-- @\"SuccessStrings\": [ \"Account creation successful\", \"Welcome to our site!\" ]@
responseInspectionBodyContains_successStrings :: Lens.Lens' ResponseInspectionBodyContains (Prelude.NonEmpty Prelude.Text)
responseInspectionBodyContains_successStrings = Lens.lens (\ResponseInspectionBodyContains' {successStrings} -> successStrings) (\s@ResponseInspectionBodyContains' {} a -> s {successStrings = a} :: ResponseInspectionBodyContains) Prelude.. Lens.coerced

-- | Strings in the body of the response that indicate a failed login or
-- account creation attempt. To be counted as a failure, the string can be
-- anywhere in the body and must be an exact match, including case. Each
-- string must be unique among the success and failure strings.
--
-- JSON example: @\"FailureStrings\": [ \"Request failed\" ]@
responseInspectionBodyContains_failureStrings :: Lens.Lens' ResponseInspectionBodyContains (Prelude.NonEmpty Prelude.Text)
responseInspectionBodyContains_failureStrings = Lens.lens (\ResponseInspectionBodyContains' {failureStrings} -> failureStrings) (\s@ResponseInspectionBodyContains' {} a -> s {failureStrings = a} :: ResponseInspectionBodyContains) Prelude.. Lens.coerced

instance Data.FromJSON ResponseInspectionBodyContains where
  parseJSON =
    Data.withObject
      "ResponseInspectionBodyContains"
      ( \x ->
          ResponseInspectionBodyContains'
            Prelude.<$> (x Data..: "SuccessStrings")
            Prelude.<*> (x Data..: "FailureStrings")
      )

instance
  Prelude.Hashable
    ResponseInspectionBodyContains
  where
  hashWithSalt
    _salt
    ResponseInspectionBodyContains' {..} =
      _salt
        `Prelude.hashWithSalt` successStrings
        `Prelude.hashWithSalt` failureStrings

instance
  Prelude.NFData
    ResponseInspectionBodyContains
  where
  rnf ResponseInspectionBodyContains' {..} =
    Prelude.rnf successStrings
      `Prelude.seq` Prelude.rnf failureStrings

instance Data.ToJSON ResponseInspectionBodyContains where
  toJSON ResponseInspectionBodyContains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SuccessStrings" Data..= successStrings),
            Prelude.Just
              ("FailureStrings" Data..= failureStrings)
          ]
      )
