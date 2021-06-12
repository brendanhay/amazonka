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
-- Module      : Network.AWS.WAF.GetRateBasedRuleManagedKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--
-- This operation returns paginated results.
module Network.AWS.WAF.GetRateBasedRuleManagedKeys
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetRateBasedRuleManagedKeys' smart constructor.
data GetRateBasedRuleManagedKeys = GetRateBasedRuleManagedKeys'
  { -- | A null value and not currently used. Do not include this in your
    -- request.
    nextMarker :: Core.Maybe Core.Text,
    -- | The @RuleId@ of the RateBasedRule for which you want to get a list of
    -- @ManagedKeys@. @RuleId@ is returned by CreateRateBasedRule and by
    -- ListRateBasedRules.
    ruleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetRateBasedRuleManagedKeys
newGetRateBasedRuleManagedKeys pRuleId_ =
  GetRateBasedRuleManagedKeys'
    { nextMarker =
        Core.Nothing,
      ruleId = pRuleId_
    }

-- | A null value and not currently used. Do not include this in your
-- request.
getRateBasedRuleManagedKeys_nextMarker :: Lens.Lens' GetRateBasedRuleManagedKeys (Core.Maybe Core.Text)
getRateBasedRuleManagedKeys_nextMarker = Lens.lens (\GetRateBasedRuleManagedKeys' {nextMarker} -> nextMarker) (\s@GetRateBasedRuleManagedKeys' {} a -> s {nextMarker = a} :: GetRateBasedRuleManagedKeys)

-- | The @RuleId@ of the RateBasedRule for which you want to get a list of
-- @ManagedKeys@. @RuleId@ is returned by CreateRateBasedRule and by
-- ListRateBasedRules.
getRateBasedRuleManagedKeys_ruleId :: Lens.Lens' GetRateBasedRuleManagedKeys Core.Text
getRateBasedRuleManagedKeys_ruleId = Lens.lens (\GetRateBasedRuleManagedKeys' {ruleId} -> ruleId) (\s@GetRateBasedRuleManagedKeys' {} a -> s {ruleId = a} :: GetRateBasedRuleManagedKeys)

instance Core.AWSPager GetRateBasedRuleManagedKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRateBasedRuleManagedKeysResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getRateBasedRuleManagedKeysResponse_managedKeys
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getRateBasedRuleManagedKeys_nextMarker
          Lens..~ rs
          Lens.^? getRateBasedRuleManagedKeysResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest GetRateBasedRuleManagedKeys where
  type
    AWSResponse GetRateBasedRuleManagedKeys =
      GetRateBasedRuleManagedKeysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRateBasedRuleManagedKeysResponse'
            Core.<$> (x Core..?> "ManagedKeys" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRateBasedRuleManagedKeys

instance Core.NFData GetRateBasedRuleManagedKeys

instance Core.ToHeaders GetRateBasedRuleManagedKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetRateBasedRuleManagedKeys" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRateBasedRuleManagedKeys where
  toJSON GetRateBasedRuleManagedKeys' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            Core.Just ("RuleId" Core..= ruleId)
          ]
      )

instance Core.ToPath GetRateBasedRuleManagedKeys where
  toPath = Core.const "/"

instance Core.ToQuery GetRateBasedRuleManagedKeys where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRateBasedRuleManagedKeysResponse' smart constructor.
data GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse'
  { -- | An array of IP addresses that currently are blocked by the specified
    -- RateBasedRule.
    managedKeys :: Core.Maybe [Core.Text],
    -- | A null value and not currently used.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetRateBasedRuleManagedKeysResponse
newGetRateBasedRuleManagedKeysResponse pHttpStatus_ =
  GetRateBasedRuleManagedKeysResponse'
    { managedKeys =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of IP addresses that currently are blocked by the specified
-- RateBasedRule.
getRateBasedRuleManagedKeysResponse_managedKeys :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Core.Maybe [Core.Text])
getRateBasedRuleManagedKeysResponse_managedKeys = Lens.lens (\GetRateBasedRuleManagedKeysResponse' {managedKeys} -> managedKeys) (\s@GetRateBasedRuleManagedKeysResponse' {} a -> s {managedKeys = a} :: GetRateBasedRuleManagedKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | A null value and not currently used.
getRateBasedRuleManagedKeysResponse_nextMarker :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Core.Maybe Core.Text)
getRateBasedRuleManagedKeysResponse_nextMarker = Lens.lens (\GetRateBasedRuleManagedKeysResponse' {nextMarker} -> nextMarker) (\s@GetRateBasedRuleManagedKeysResponse' {} a -> s {nextMarker = a} :: GetRateBasedRuleManagedKeysResponse)

-- | The response's http status code.
getRateBasedRuleManagedKeysResponse_httpStatus :: Lens.Lens' GetRateBasedRuleManagedKeysResponse Core.Int
getRateBasedRuleManagedKeysResponse_httpStatus = Lens.lens (\GetRateBasedRuleManagedKeysResponse' {httpStatus} -> httpStatus) (\s@GetRateBasedRuleManagedKeysResponse' {} a -> s {httpStatus = a} :: GetRateBasedRuleManagedKeysResponse)

instance
  Core.NFData
    GetRateBasedRuleManagedKeysResponse
