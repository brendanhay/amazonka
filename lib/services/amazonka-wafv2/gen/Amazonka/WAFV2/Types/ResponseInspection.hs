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
-- Module      : Amazonka.WAFV2.Types.ResponseInspection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResponseInspection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ResponseInspectionBodyContains
import Amazonka.WAFV2.Types.ResponseInspectionHeader
import Amazonka.WAFV2.Types.ResponseInspectionJson
import Amazonka.WAFV2.Types.ResponseInspectionStatusCode

-- | The criteria for inspecting responses to login requests and account
-- creation requests, used by the ATP and ACFP rule groups to track login
-- and account creation success and failure rates.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- The rule groups evaluates the responses that your protected resources
-- send back to client login and account creation attempts, keeping count
-- of successful and failed attempts from each IP address and client
-- session. Using this information, the rule group labels and mitigates
-- requests from client sessions and IP addresses with too much suspicious
-- activity in a short amount of time.
--
-- This is part of the @AWSManagedRulesATPRuleSet@ and
-- @AWSManagedRulesACFPRuleSet@ configurations in @ManagedRuleGroupConfig@.
--
-- Enable response inspection by configuring exactly one component of the
-- response to inspect, for example, @Header@ or @StatusCode@. You can\'t
-- configure more than one component for inspection. If you don\'t
-- configure any of the response inspection options, response inspection is
-- disabled.
--
-- /See:/ 'newResponseInspection' smart constructor.
data ResponseInspection = ResponseInspection'
  { -- | Configures inspection of the response body for success and failure
    -- indicators. WAF can inspect the first 65,536 bytes (64 KB) of the
    -- response body.
    bodyContains :: Prelude.Maybe ResponseInspectionBodyContains,
    -- | Configures inspection of the response header for success and failure
    -- indicators.
    header :: Prelude.Maybe ResponseInspectionHeader,
    -- | Configures inspection of the response JSON for success and failure
    -- indicators. WAF can inspect the first 65,536 bytes (64 KB) of the
    -- response JSON.
    json :: Prelude.Maybe ResponseInspectionJson,
    -- | Configures inspection of the response status code for success and
    -- failure indicators.
    statusCode :: Prelude.Maybe ResponseInspectionStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseInspection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bodyContains', 'responseInspection_bodyContains' - Configures inspection of the response body for success and failure
-- indicators. WAF can inspect the first 65,536 bytes (64 KB) of the
-- response body.
--
-- 'header', 'responseInspection_header' - Configures inspection of the response header for success and failure
-- indicators.
--
-- 'json', 'responseInspection_json' - Configures inspection of the response JSON for success and failure
-- indicators. WAF can inspect the first 65,536 bytes (64 KB) of the
-- response JSON.
--
-- 'statusCode', 'responseInspection_statusCode' - Configures inspection of the response status code for success and
-- failure indicators.
newResponseInspection ::
  ResponseInspection
newResponseInspection =
  ResponseInspection'
    { bodyContains = Prelude.Nothing,
      header = Prelude.Nothing,
      json = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Configures inspection of the response body for success and failure
-- indicators. WAF can inspect the first 65,536 bytes (64 KB) of the
-- response body.
responseInspection_bodyContains :: Lens.Lens' ResponseInspection (Prelude.Maybe ResponseInspectionBodyContains)
responseInspection_bodyContains = Lens.lens (\ResponseInspection' {bodyContains} -> bodyContains) (\s@ResponseInspection' {} a -> s {bodyContains = a} :: ResponseInspection)

-- | Configures inspection of the response header for success and failure
-- indicators.
responseInspection_header :: Lens.Lens' ResponseInspection (Prelude.Maybe ResponseInspectionHeader)
responseInspection_header = Lens.lens (\ResponseInspection' {header} -> header) (\s@ResponseInspection' {} a -> s {header = a} :: ResponseInspection)

-- | Configures inspection of the response JSON for success and failure
-- indicators. WAF can inspect the first 65,536 bytes (64 KB) of the
-- response JSON.
responseInspection_json :: Lens.Lens' ResponseInspection (Prelude.Maybe ResponseInspectionJson)
responseInspection_json = Lens.lens (\ResponseInspection' {json} -> json) (\s@ResponseInspection' {} a -> s {json = a} :: ResponseInspection)

-- | Configures inspection of the response status code for success and
-- failure indicators.
responseInspection_statusCode :: Lens.Lens' ResponseInspection (Prelude.Maybe ResponseInspectionStatusCode)
responseInspection_statusCode = Lens.lens (\ResponseInspection' {statusCode} -> statusCode) (\s@ResponseInspection' {} a -> s {statusCode = a} :: ResponseInspection)

instance Data.FromJSON ResponseInspection where
  parseJSON =
    Data.withObject
      "ResponseInspection"
      ( \x ->
          ResponseInspection'
            Prelude.<$> (x Data..:? "BodyContains")
            Prelude.<*> (x Data..:? "Header")
            Prelude.<*> (x Data..:? "Json")
            Prelude.<*> (x Data..:? "StatusCode")
      )

instance Prelude.Hashable ResponseInspection where
  hashWithSalt _salt ResponseInspection' {..} =
    _salt
      `Prelude.hashWithSalt` bodyContains
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` json
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData ResponseInspection where
  rnf ResponseInspection' {..} =
    Prelude.rnf bodyContains
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf json
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToJSON ResponseInspection where
  toJSON ResponseInspection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BodyContains" Data..=) Prelude.<$> bodyContains,
            ("Header" Data..=) Prelude.<$> header,
            ("Json" Data..=) Prelude.<$> json,
            ("StatusCode" Data..=) Prelude.<$> statusCode
          ]
      )
