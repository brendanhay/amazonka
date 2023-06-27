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
-- Module      : Amazonka.WAFV2.Types.ResponseInspectionStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResponseInspectionStatusCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures inspection of the response status code. This is part of the
-- @ResponseInspection@ configuration for @AWSManagedRulesATPRuleSet@ and
-- @AWSManagedRulesACFPRuleSet@.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- /See:/ 'newResponseInspectionStatusCode' smart constructor.
data ResponseInspectionStatusCode = ResponseInspectionStatusCode'
  { -- | Status codes in the response that indicate a successful login or account
    -- creation attempt. To be counted as a success, the response status code
    -- must match one of these. Each code must be unique among the success and
    -- failure status codes.
    --
    -- JSON example: @\"SuccessCodes\": [ 200, 201 ]@
    successCodes :: Prelude.NonEmpty Prelude.Natural,
    -- | Status codes in the response that indicate a failed login or account
    -- creation attempt. To be counted as a failure, the response status code
    -- must match one of these. Each code must be unique among the success and
    -- failure status codes.
    --
    -- JSON example: @\"FailureCodes\": [ 400, 404 ]@
    failureCodes :: Prelude.NonEmpty Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseInspectionStatusCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successCodes', 'responseInspectionStatusCode_successCodes' - Status codes in the response that indicate a successful login or account
-- creation attempt. To be counted as a success, the response status code
-- must match one of these. Each code must be unique among the success and
-- failure status codes.
--
-- JSON example: @\"SuccessCodes\": [ 200, 201 ]@
--
-- 'failureCodes', 'responseInspectionStatusCode_failureCodes' - Status codes in the response that indicate a failed login or account
-- creation attempt. To be counted as a failure, the response status code
-- must match one of these. Each code must be unique among the success and
-- failure status codes.
--
-- JSON example: @\"FailureCodes\": [ 400, 404 ]@
newResponseInspectionStatusCode ::
  -- | 'successCodes'
  Prelude.NonEmpty Prelude.Natural ->
  -- | 'failureCodes'
  Prelude.NonEmpty Prelude.Natural ->
  ResponseInspectionStatusCode
newResponseInspectionStatusCode
  pSuccessCodes_
  pFailureCodes_ =
    ResponseInspectionStatusCode'
      { successCodes =
          Lens.coerced Lens.# pSuccessCodes_,
        failureCodes =
          Lens.coerced Lens.# pFailureCodes_
      }

-- | Status codes in the response that indicate a successful login or account
-- creation attempt. To be counted as a success, the response status code
-- must match one of these. Each code must be unique among the success and
-- failure status codes.
--
-- JSON example: @\"SuccessCodes\": [ 200, 201 ]@
responseInspectionStatusCode_successCodes :: Lens.Lens' ResponseInspectionStatusCode (Prelude.NonEmpty Prelude.Natural)
responseInspectionStatusCode_successCodes = Lens.lens (\ResponseInspectionStatusCode' {successCodes} -> successCodes) (\s@ResponseInspectionStatusCode' {} a -> s {successCodes = a} :: ResponseInspectionStatusCode) Prelude.. Lens.coerced

-- | Status codes in the response that indicate a failed login or account
-- creation attempt. To be counted as a failure, the response status code
-- must match one of these. Each code must be unique among the success and
-- failure status codes.
--
-- JSON example: @\"FailureCodes\": [ 400, 404 ]@
responseInspectionStatusCode_failureCodes :: Lens.Lens' ResponseInspectionStatusCode (Prelude.NonEmpty Prelude.Natural)
responseInspectionStatusCode_failureCodes = Lens.lens (\ResponseInspectionStatusCode' {failureCodes} -> failureCodes) (\s@ResponseInspectionStatusCode' {} a -> s {failureCodes = a} :: ResponseInspectionStatusCode) Prelude.. Lens.coerced

instance Data.FromJSON ResponseInspectionStatusCode where
  parseJSON =
    Data.withObject
      "ResponseInspectionStatusCode"
      ( \x ->
          ResponseInspectionStatusCode'
            Prelude.<$> (x Data..: "SuccessCodes")
            Prelude.<*> (x Data..: "FailureCodes")
      )

instance
  Prelude.Hashable
    ResponseInspectionStatusCode
  where
  hashWithSalt _salt ResponseInspectionStatusCode' {..} =
    _salt
      `Prelude.hashWithSalt` successCodes
      `Prelude.hashWithSalt` failureCodes

instance Prelude.NFData ResponseInspectionStatusCode where
  rnf ResponseInspectionStatusCode' {..} =
    Prelude.rnf successCodes
      `Prelude.seq` Prelude.rnf failureCodes

instance Data.ToJSON ResponseInspectionStatusCode where
  toJSON ResponseInspectionStatusCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SuccessCodes" Data..= successCodes),
            Prelude.Just ("FailureCodes" Data..= failureCodes)
          ]
      )
