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
-- Module      : Amazonka.Evidently.TestSegmentPattern
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to test a rules pattern that you plan to use to
-- create an audience segment. For more information about segments, see
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_CreateSegment.html CreateSegment>.
module Amazonka.Evidently.TestSegmentPattern
  ( -- * Creating a Request
    TestSegmentPattern (..),
    newTestSegmentPattern,

    -- * Request Lenses
    testSegmentPattern_pattern,
    testSegmentPattern_payload,

    -- * Destructuring the Response
    TestSegmentPatternResponse (..),
    newTestSegmentPatternResponse,

    -- * Response Lenses
    testSegmentPatternResponse_httpStatus,
    testSegmentPatternResponse_match,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestSegmentPattern' smart constructor.
data TestSegmentPattern = TestSegmentPattern'
  { -- | The pattern to test.
    pattern' :: Prelude.Text,
    -- | A sample @evaluationContext@ JSON block to test against the specified
    -- pattern.
    payload :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSegmentPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pattern'', 'testSegmentPattern_pattern' - The pattern to test.
--
-- 'payload', 'testSegmentPattern_payload' - A sample @evaluationContext@ JSON block to test against the specified
-- pattern.
newTestSegmentPattern ::
  -- | 'pattern''
  Prelude.Text ->
  -- | 'payload'
  Prelude.Text ->
  TestSegmentPattern
newTestSegmentPattern pPattern_ pPayload_ =
  TestSegmentPattern'
    { pattern' = pPattern_,
      payload = pPayload_
    }

-- | The pattern to test.
testSegmentPattern_pattern :: Lens.Lens' TestSegmentPattern Prelude.Text
testSegmentPattern_pattern = Lens.lens (\TestSegmentPattern' {pattern'} -> pattern') (\s@TestSegmentPattern' {} a -> s {pattern' = a} :: TestSegmentPattern)

-- | A sample @evaluationContext@ JSON block to test against the specified
-- pattern.
testSegmentPattern_payload :: Lens.Lens' TestSegmentPattern Prelude.Text
testSegmentPattern_payload = Lens.lens (\TestSegmentPattern' {payload} -> payload) (\s@TestSegmentPattern' {} a -> s {payload = a} :: TestSegmentPattern)

instance Core.AWSRequest TestSegmentPattern where
  type
    AWSResponse TestSegmentPattern =
      TestSegmentPatternResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestSegmentPatternResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "match")
      )

instance Prelude.Hashable TestSegmentPattern where
  hashWithSalt _salt TestSegmentPattern' {..} =
    _salt `Prelude.hashWithSalt` pattern'
      `Prelude.hashWithSalt` payload

instance Prelude.NFData TestSegmentPattern where
  rnf TestSegmentPattern' {..} =
    Prelude.rnf pattern'
      `Prelude.seq` Prelude.rnf payload

instance Core.ToHeaders TestSegmentPattern where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TestSegmentPattern where
  toJSON TestSegmentPattern' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pattern" Core..= pattern'),
            Prelude.Just ("payload" Core..= payload)
          ]
      )

instance Core.ToPath TestSegmentPattern where
  toPath = Prelude.const "/test-segment-pattern"

instance Core.ToQuery TestSegmentPattern where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestSegmentPatternResponse' smart constructor.
data TestSegmentPatternResponse = TestSegmentPatternResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns @true@ if the pattern matches the payload.
    match :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSegmentPatternResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'testSegmentPatternResponse_httpStatus' - The response's http status code.
--
-- 'match', 'testSegmentPatternResponse_match' - Returns @true@ if the pattern matches the payload.
newTestSegmentPatternResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'match'
  Prelude.Bool ->
  TestSegmentPatternResponse
newTestSegmentPatternResponse pHttpStatus_ pMatch_ =
  TestSegmentPatternResponse'
    { httpStatus =
        pHttpStatus_,
      match = pMatch_
    }

-- | The response's http status code.
testSegmentPatternResponse_httpStatus :: Lens.Lens' TestSegmentPatternResponse Prelude.Int
testSegmentPatternResponse_httpStatus = Lens.lens (\TestSegmentPatternResponse' {httpStatus} -> httpStatus) (\s@TestSegmentPatternResponse' {} a -> s {httpStatus = a} :: TestSegmentPatternResponse)

-- | Returns @true@ if the pattern matches the payload.
testSegmentPatternResponse_match :: Lens.Lens' TestSegmentPatternResponse Prelude.Bool
testSegmentPatternResponse_match = Lens.lens (\TestSegmentPatternResponse' {match} -> match) (\s@TestSegmentPatternResponse' {} a -> s {match = a} :: TestSegmentPatternResponse)

instance Prelude.NFData TestSegmentPatternResponse where
  rnf TestSegmentPatternResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf match
