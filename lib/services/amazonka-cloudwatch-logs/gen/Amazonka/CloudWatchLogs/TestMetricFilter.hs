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
-- Module      : Amazonka.CloudWatchLogs.TestMetricFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the filter pattern of a metric filter against a sample of log
-- event messages. You can use this operation to validate the correctness
-- of a metric filter pattern.
module Amazonka.CloudWatchLogs.TestMetricFilter
  ( -- * Creating a Request
    TestMetricFilter (..),
    newTestMetricFilter,

    -- * Request Lenses
    testMetricFilter_filterPattern,
    testMetricFilter_logEventMessages,

    -- * Destructuring the Response
    TestMetricFilterResponse (..),
    newTestMetricFilterResponse,

    -- * Response Lenses
    testMetricFilterResponse_matches,
    testMetricFilterResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestMetricFilter' smart constructor.
data TestMetricFilter = TestMetricFilter'
  { filterPattern :: Prelude.Text,
    -- | The log event messages to test.
    logEventMessages :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestMetricFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterPattern', 'testMetricFilter_filterPattern' - Undocumented member.
--
-- 'logEventMessages', 'testMetricFilter_logEventMessages' - The log event messages to test.
newTestMetricFilter ::
  -- | 'filterPattern'
  Prelude.Text ->
  -- | 'logEventMessages'
  Prelude.NonEmpty Prelude.Text ->
  TestMetricFilter
newTestMetricFilter
  pFilterPattern_
  pLogEventMessages_ =
    TestMetricFilter'
      { filterPattern = pFilterPattern_,
        logEventMessages =
          Lens.coerced Lens.# pLogEventMessages_
      }

-- | Undocumented member.
testMetricFilter_filterPattern :: Lens.Lens' TestMetricFilter Prelude.Text
testMetricFilter_filterPattern = Lens.lens (\TestMetricFilter' {filterPattern} -> filterPattern) (\s@TestMetricFilter' {} a -> s {filterPattern = a} :: TestMetricFilter)

-- | The log event messages to test.
testMetricFilter_logEventMessages :: Lens.Lens' TestMetricFilter (Prelude.NonEmpty Prelude.Text)
testMetricFilter_logEventMessages = Lens.lens (\TestMetricFilter' {logEventMessages} -> logEventMessages) (\s@TestMetricFilter' {} a -> s {logEventMessages = a} :: TestMetricFilter) Prelude.. Lens.coerced

instance Core.AWSRequest TestMetricFilter where
  type
    AWSResponse TestMetricFilter =
      TestMetricFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestMetricFilterResponse'
            Prelude.<$> (x Data..?> "matches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestMetricFilter where
  hashWithSalt _salt TestMetricFilter' {..} =
    _salt
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` logEventMessages

instance Prelude.NFData TestMetricFilter where
  rnf TestMetricFilter' {..} =
    Prelude.rnf filterPattern
      `Prelude.seq` Prelude.rnf logEventMessages

instance Data.ToHeaders TestMetricFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.TestMetricFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TestMetricFilter where
  toJSON TestMetricFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("filterPattern" Data..= filterPattern),
            Prelude.Just
              ("logEventMessages" Data..= logEventMessages)
          ]
      )

instance Data.ToPath TestMetricFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery TestMetricFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestMetricFilterResponse' smart constructor.
data TestMetricFilterResponse = TestMetricFilterResponse'
  { -- | The matched events.
    matches :: Prelude.Maybe [MetricFilterMatchRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestMetricFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matches', 'testMetricFilterResponse_matches' - The matched events.
--
-- 'httpStatus', 'testMetricFilterResponse_httpStatus' - The response's http status code.
newTestMetricFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestMetricFilterResponse
newTestMetricFilterResponse pHttpStatus_ =
  TestMetricFilterResponse'
    { matches =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The matched events.
testMetricFilterResponse_matches :: Lens.Lens' TestMetricFilterResponse (Prelude.Maybe [MetricFilterMatchRecord])
testMetricFilterResponse_matches = Lens.lens (\TestMetricFilterResponse' {matches} -> matches) (\s@TestMetricFilterResponse' {} a -> s {matches = a} :: TestMetricFilterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
testMetricFilterResponse_httpStatus :: Lens.Lens' TestMetricFilterResponse Prelude.Int
testMetricFilterResponse_httpStatus = Lens.lens (\TestMetricFilterResponse' {httpStatus} -> httpStatus) (\s@TestMetricFilterResponse' {} a -> s {httpStatus = a} :: TestMetricFilterResponse)

instance Prelude.NFData TestMetricFilterResponse where
  rnf TestMetricFilterResponse' {..} =
    Prelude.rnf matches
      `Prelude.seq` Prelude.rnf httpStatus
