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
-- Module      : Amazonka.WAFV2.Types.ResponseInspectionHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResponseInspectionHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures inspection of the response header. This is part of the
-- @ResponseInspection@ configuration for @AWSManagedRulesATPRuleSet@ and
-- @AWSManagedRulesACFPRuleSet@.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- /See:/ 'newResponseInspectionHeader' smart constructor.
data ResponseInspectionHeader = ResponseInspectionHeader'
  { -- | The name of the header to match against. The name must be an exact
    -- match, including case.
    --
    -- JSON example: @\"Name\": [ \"RequestResult\" ]@
    name :: Prelude.Text,
    -- | Values in the response header with the specified name that indicate a
    -- successful login or account creation attempt. To be counted as a
    -- success, the value must be an exact match, including case. Each value
    -- must be unique among the success and failure values.
    --
    -- JSON examples:
    -- @\"SuccessValues\": [ \"LoginPassed\", \"Successful login\" ]@ and
    -- @\"SuccessValues\": [ \"AccountCreated\", \"Successful account creation\" ]@
    successValues :: Prelude.NonEmpty Prelude.Text,
    -- | Values in the response header with the specified name that indicate a
    -- failed login or account creation attempt. To be counted as a failure,
    -- the value must be an exact match, including case. Each value must be
    -- unique among the success and failure values.
    --
    -- JSON examples:
    -- @\"FailureValues\": [ \"LoginFailed\", \"Failed login\" ]@ and
    -- @\"FailureValues\": [ \"AccountCreationFailed\" ]@
    failureValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseInspectionHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'responseInspectionHeader_name' - The name of the header to match against. The name must be an exact
-- match, including case.
--
-- JSON example: @\"Name\": [ \"RequestResult\" ]@
--
-- 'successValues', 'responseInspectionHeader_successValues' - Values in the response header with the specified name that indicate a
-- successful login or account creation attempt. To be counted as a
-- success, the value must be an exact match, including case. Each value
-- must be unique among the success and failure values.
--
-- JSON examples:
-- @\"SuccessValues\": [ \"LoginPassed\", \"Successful login\" ]@ and
-- @\"SuccessValues\": [ \"AccountCreated\", \"Successful account creation\" ]@
--
-- 'failureValues', 'responseInspectionHeader_failureValues' - Values in the response header with the specified name that indicate a
-- failed login or account creation attempt. To be counted as a failure,
-- the value must be an exact match, including case. Each value must be
-- unique among the success and failure values.
--
-- JSON examples:
-- @\"FailureValues\": [ \"LoginFailed\", \"Failed login\" ]@ and
-- @\"FailureValues\": [ \"AccountCreationFailed\" ]@
newResponseInspectionHeader ::
  -- | 'name'
  Prelude.Text ->
  -- | 'successValues'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'failureValues'
  Prelude.NonEmpty Prelude.Text ->
  ResponseInspectionHeader
newResponseInspectionHeader
  pName_
  pSuccessValues_
  pFailureValues_ =
    ResponseInspectionHeader'
      { name = pName_,
        successValues =
          Lens.coerced Lens.# pSuccessValues_,
        failureValues =
          Lens.coerced Lens.# pFailureValues_
      }

-- | The name of the header to match against. The name must be an exact
-- match, including case.
--
-- JSON example: @\"Name\": [ \"RequestResult\" ]@
responseInspectionHeader_name :: Lens.Lens' ResponseInspectionHeader Prelude.Text
responseInspectionHeader_name = Lens.lens (\ResponseInspectionHeader' {name} -> name) (\s@ResponseInspectionHeader' {} a -> s {name = a} :: ResponseInspectionHeader)

-- | Values in the response header with the specified name that indicate a
-- successful login or account creation attempt. To be counted as a
-- success, the value must be an exact match, including case. Each value
-- must be unique among the success and failure values.
--
-- JSON examples:
-- @\"SuccessValues\": [ \"LoginPassed\", \"Successful login\" ]@ and
-- @\"SuccessValues\": [ \"AccountCreated\", \"Successful account creation\" ]@
responseInspectionHeader_successValues :: Lens.Lens' ResponseInspectionHeader (Prelude.NonEmpty Prelude.Text)
responseInspectionHeader_successValues = Lens.lens (\ResponseInspectionHeader' {successValues} -> successValues) (\s@ResponseInspectionHeader' {} a -> s {successValues = a} :: ResponseInspectionHeader) Prelude.. Lens.coerced

-- | Values in the response header with the specified name that indicate a
-- failed login or account creation attempt. To be counted as a failure,
-- the value must be an exact match, including case. Each value must be
-- unique among the success and failure values.
--
-- JSON examples:
-- @\"FailureValues\": [ \"LoginFailed\", \"Failed login\" ]@ and
-- @\"FailureValues\": [ \"AccountCreationFailed\" ]@
responseInspectionHeader_failureValues :: Lens.Lens' ResponseInspectionHeader (Prelude.NonEmpty Prelude.Text)
responseInspectionHeader_failureValues = Lens.lens (\ResponseInspectionHeader' {failureValues} -> failureValues) (\s@ResponseInspectionHeader' {} a -> s {failureValues = a} :: ResponseInspectionHeader) Prelude.. Lens.coerced

instance Data.FromJSON ResponseInspectionHeader where
  parseJSON =
    Data.withObject
      "ResponseInspectionHeader"
      ( \x ->
          ResponseInspectionHeader'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "SuccessValues")
            Prelude.<*> (x Data..: "FailureValues")
      )

instance Prelude.Hashable ResponseInspectionHeader where
  hashWithSalt _salt ResponseInspectionHeader' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` successValues
      `Prelude.hashWithSalt` failureValues

instance Prelude.NFData ResponseInspectionHeader where
  rnf ResponseInspectionHeader' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf successValues
      `Prelude.seq` Prelude.rnf failureValues

instance Data.ToJSON ResponseInspectionHeader where
  toJSON ResponseInspectionHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SuccessValues" Data..= successValues),
            Prelude.Just
              ("FailureValues" Data..= failureValues)
          ]
      )
