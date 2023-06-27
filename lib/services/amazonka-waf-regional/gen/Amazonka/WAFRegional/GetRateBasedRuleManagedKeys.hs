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
-- Module      : Amazonka.WAFRegional.GetRateBasedRuleManagedKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns an array of IP addresses currently being blocked by the
-- RateBasedRule that is specified by the @RuleId@. The maximum number of
-- managed keys that will be blocked is 10,000. If more than 10,000
-- addresses exceed the rate limit, the 10,000 addresses with the highest
-- rates will be blocked.
module Amazonka.WAFRegional.GetRateBasedRuleManagedKeys
  ( -- * Creating a Request
    GetRateBasedRuleManagedKeys (..),
    newGetRateBasedRuleManagedKeys,

    -- * Request Lenses
    getRateBasedRuleManagedKeys_nextMarker,
    getRateBasedRuleManagedKeys_ruleId,

    -- * Destructuring the Response
    GetRateBasedRuleManagedKeysResponse (..),
    newGetRateBasedRuleManagedKeysResponse,

    -- * Response Lenses
    getRateBasedRuleManagedKeysResponse_managedKeys,
    getRateBasedRuleManagedKeysResponse_nextMarker,
    getRateBasedRuleManagedKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newGetRateBasedRuleManagedKeys' smart constructor.
data GetRateBasedRuleManagedKeys = GetRateBasedRuleManagedKeys'
  { -- | A null value and not currently used. Do not include this in your
    -- request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The @RuleId@ of the RateBasedRule for which you want to get a list of
    -- @ManagedKeys@. @RuleId@ is returned by CreateRateBasedRule and by
    -- ListRateBasedRules.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRateBasedRuleManagedKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'getRateBasedRuleManagedKeys_nextMarker' - A null value and not currently used. Do not include this in your
-- request.
--
-- 'ruleId', 'getRateBasedRuleManagedKeys_ruleId' - The @RuleId@ of the RateBasedRule for which you want to get a list of
-- @ManagedKeys@. @RuleId@ is returned by CreateRateBasedRule and by
-- ListRateBasedRules.
newGetRateBasedRuleManagedKeys ::
  -- | 'ruleId'
  Prelude.Text ->
  GetRateBasedRuleManagedKeys
newGetRateBasedRuleManagedKeys pRuleId_ =
  GetRateBasedRuleManagedKeys'
    { nextMarker =
        Prelude.Nothing,
      ruleId = pRuleId_
    }

-- | A null value and not currently used. Do not include this in your
-- request.
getRateBasedRuleManagedKeys_nextMarker :: Lens.Lens' GetRateBasedRuleManagedKeys (Prelude.Maybe Prelude.Text)
getRateBasedRuleManagedKeys_nextMarker = Lens.lens (\GetRateBasedRuleManagedKeys' {nextMarker} -> nextMarker) (\s@GetRateBasedRuleManagedKeys' {} a -> s {nextMarker = a} :: GetRateBasedRuleManagedKeys)

-- | The @RuleId@ of the RateBasedRule for which you want to get a list of
-- @ManagedKeys@. @RuleId@ is returned by CreateRateBasedRule and by
-- ListRateBasedRules.
getRateBasedRuleManagedKeys_ruleId :: Lens.Lens' GetRateBasedRuleManagedKeys Prelude.Text
getRateBasedRuleManagedKeys_ruleId = Lens.lens (\GetRateBasedRuleManagedKeys' {ruleId} -> ruleId) (\s@GetRateBasedRuleManagedKeys' {} a -> s {ruleId = a} :: GetRateBasedRuleManagedKeys)

instance Core.AWSRequest GetRateBasedRuleManagedKeys where
  type
    AWSResponse GetRateBasedRuleManagedKeys =
      GetRateBasedRuleManagedKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRateBasedRuleManagedKeysResponse'
            Prelude.<$> (x Data..?> "ManagedKeys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRateBasedRuleManagedKeys where
  hashWithSalt _salt GetRateBasedRuleManagedKeys' {..} =
    _salt
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` ruleId

instance Prelude.NFData GetRateBasedRuleManagedKeys where
  rnf GetRateBasedRuleManagedKeys' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf ruleId

instance Data.ToHeaders GetRateBasedRuleManagedKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.GetRateBasedRuleManagedKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRateBasedRuleManagedKeys where
  toJSON GetRateBasedRuleManagedKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("RuleId" Data..= ruleId)
          ]
      )

instance Data.ToPath GetRateBasedRuleManagedKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRateBasedRuleManagedKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRateBasedRuleManagedKeysResponse' smart constructor.
data GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse'
  { -- | An array of IP addresses that currently are blocked by the specified
    -- RateBasedRule.
    managedKeys :: Prelude.Maybe [Prelude.Text],
    -- | A null value and not currently used.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRateBasedRuleManagedKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedKeys', 'getRateBasedRuleManagedKeysResponse_managedKeys' - An array of IP addresses that currently are blocked by the specified
-- RateBasedRule.
--
-- 'nextMarker', 'getRateBasedRuleManagedKeysResponse_nextMarker' - A null value and not currently used.
--
-- 'httpStatus', 'getRateBasedRuleManagedKeysResponse_httpStatus' - The response's http status code.
newGetRateBasedRuleManagedKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRateBasedRuleManagedKeysResponse
newGetRateBasedRuleManagedKeysResponse pHttpStatus_ =
  GetRateBasedRuleManagedKeysResponse'
    { managedKeys =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of IP addresses that currently are blocked by the specified
-- RateBasedRule.
getRateBasedRuleManagedKeysResponse_managedKeys :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Prelude.Maybe [Prelude.Text])
getRateBasedRuleManagedKeysResponse_managedKeys = Lens.lens (\GetRateBasedRuleManagedKeysResponse' {managedKeys} -> managedKeys) (\s@GetRateBasedRuleManagedKeysResponse' {} a -> s {managedKeys = a} :: GetRateBasedRuleManagedKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | A null value and not currently used.
getRateBasedRuleManagedKeysResponse_nextMarker :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Prelude.Maybe Prelude.Text)
getRateBasedRuleManagedKeysResponse_nextMarker = Lens.lens (\GetRateBasedRuleManagedKeysResponse' {nextMarker} -> nextMarker) (\s@GetRateBasedRuleManagedKeysResponse' {} a -> s {nextMarker = a} :: GetRateBasedRuleManagedKeysResponse)

-- | The response's http status code.
getRateBasedRuleManagedKeysResponse_httpStatus :: Lens.Lens' GetRateBasedRuleManagedKeysResponse Prelude.Int
getRateBasedRuleManagedKeysResponse_httpStatus = Lens.lens (\GetRateBasedRuleManagedKeysResponse' {httpStatus} -> httpStatus) (\s@GetRateBasedRuleManagedKeysResponse' {} a -> s {httpStatus = a} :: GetRateBasedRuleManagedKeysResponse)

instance
  Prelude.NFData
    GetRateBasedRuleManagedKeysResponse
  where
  rnf GetRateBasedRuleManagedKeysResponse' {..} =
    Prelude.rnf managedKeys
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
