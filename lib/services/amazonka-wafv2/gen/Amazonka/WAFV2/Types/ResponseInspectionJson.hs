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
-- Module      : Amazonka.WAFV2.Types.ResponseInspectionJson
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResponseInspectionJson where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures inspection of the response JSON. WAF can inspect the first
-- 65,536 bytes (64 KB) of the response JSON. This is part of the
-- @ResponseInspection@ configuration for @AWSManagedRulesATPRuleSet@ and
-- @AWSManagedRulesACFPRuleSet@.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- /See:/ 'newResponseInspectionJson' smart constructor.
data ResponseInspectionJson = ResponseInspectionJson'
  { -- | The identifier for the value to match against in the JSON. The
    -- identifier must be an exact match, including case.
    --
    -- JSON examples: @\"Identifier\": [ \"\/login\/success\" ]@ and
    -- @\"Identifier\": [ \"\/sign-up\/success\" ]@
    identifier :: Prelude.Text,
    -- | Values for the specified identifier in the response JSON that indicate a
    -- successful login or account creation attempt. To be counted as a
    -- success, the value must be an exact match, including case. Each value
    -- must be unique among the success and failure values.
    --
    -- JSON example: @\"SuccessValues\": [ \"True\", \"Succeeded\" ]@
    successValues :: Prelude.NonEmpty Prelude.Text,
    -- | Values for the specified identifier in the response JSON that indicate a
    -- failed login or account creation attempt. To be counted as a failure,
    -- the value must be an exact match, including case. Each value must be
    -- unique among the success and failure values.
    --
    -- JSON example: @\"FailureValues\": [ \"False\", \"Failed\" ]@
    failureValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseInspectionJson' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'responseInspectionJson_identifier' - The identifier for the value to match against in the JSON. The
-- identifier must be an exact match, including case.
--
-- JSON examples: @\"Identifier\": [ \"\/login\/success\" ]@ and
-- @\"Identifier\": [ \"\/sign-up\/success\" ]@
--
-- 'successValues', 'responseInspectionJson_successValues' - Values for the specified identifier in the response JSON that indicate a
-- successful login or account creation attempt. To be counted as a
-- success, the value must be an exact match, including case. Each value
-- must be unique among the success and failure values.
--
-- JSON example: @\"SuccessValues\": [ \"True\", \"Succeeded\" ]@
--
-- 'failureValues', 'responseInspectionJson_failureValues' - Values for the specified identifier in the response JSON that indicate a
-- failed login or account creation attempt. To be counted as a failure,
-- the value must be an exact match, including case. Each value must be
-- unique among the success and failure values.
--
-- JSON example: @\"FailureValues\": [ \"False\", \"Failed\" ]@
newResponseInspectionJson ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'successValues'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'failureValues'
  Prelude.NonEmpty Prelude.Text ->
  ResponseInspectionJson
newResponseInspectionJson
  pIdentifier_
  pSuccessValues_
  pFailureValues_ =
    ResponseInspectionJson'
      { identifier = pIdentifier_,
        successValues = Lens.coerced Lens.# pSuccessValues_,
        failureValues = Lens.coerced Lens.# pFailureValues_
      }

-- | The identifier for the value to match against in the JSON. The
-- identifier must be an exact match, including case.
--
-- JSON examples: @\"Identifier\": [ \"\/login\/success\" ]@ and
-- @\"Identifier\": [ \"\/sign-up\/success\" ]@
responseInspectionJson_identifier :: Lens.Lens' ResponseInspectionJson Prelude.Text
responseInspectionJson_identifier = Lens.lens (\ResponseInspectionJson' {identifier} -> identifier) (\s@ResponseInspectionJson' {} a -> s {identifier = a} :: ResponseInspectionJson)

-- | Values for the specified identifier in the response JSON that indicate a
-- successful login or account creation attempt. To be counted as a
-- success, the value must be an exact match, including case. Each value
-- must be unique among the success and failure values.
--
-- JSON example: @\"SuccessValues\": [ \"True\", \"Succeeded\" ]@
responseInspectionJson_successValues :: Lens.Lens' ResponseInspectionJson (Prelude.NonEmpty Prelude.Text)
responseInspectionJson_successValues = Lens.lens (\ResponseInspectionJson' {successValues} -> successValues) (\s@ResponseInspectionJson' {} a -> s {successValues = a} :: ResponseInspectionJson) Prelude.. Lens.coerced

-- | Values for the specified identifier in the response JSON that indicate a
-- failed login or account creation attempt. To be counted as a failure,
-- the value must be an exact match, including case. Each value must be
-- unique among the success and failure values.
--
-- JSON example: @\"FailureValues\": [ \"False\", \"Failed\" ]@
responseInspectionJson_failureValues :: Lens.Lens' ResponseInspectionJson (Prelude.NonEmpty Prelude.Text)
responseInspectionJson_failureValues = Lens.lens (\ResponseInspectionJson' {failureValues} -> failureValues) (\s@ResponseInspectionJson' {} a -> s {failureValues = a} :: ResponseInspectionJson) Prelude.. Lens.coerced

instance Data.FromJSON ResponseInspectionJson where
  parseJSON =
    Data.withObject
      "ResponseInspectionJson"
      ( \x ->
          ResponseInspectionJson'
            Prelude.<$> (x Data..: "Identifier")
            Prelude.<*> (x Data..: "SuccessValues")
            Prelude.<*> (x Data..: "FailureValues")
      )

instance Prelude.Hashable ResponseInspectionJson where
  hashWithSalt _salt ResponseInspectionJson' {..} =
    _salt
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` successValues
      `Prelude.hashWithSalt` failureValues

instance Prelude.NFData ResponseInspectionJson where
  rnf ResponseInspectionJson' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf successValues
      `Prelude.seq` Prelude.rnf failureValues

instance Data.ToJSON ResponseInspectionJson where
  toJSON ResponseInspectionJson' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Identifier" Data..= identifier),
            Prelude.Just ("SuccessValues" Data..= successValues),
            Prelude.Just
              ("FailureValues" Data..= failureValues)
          ]
      )
