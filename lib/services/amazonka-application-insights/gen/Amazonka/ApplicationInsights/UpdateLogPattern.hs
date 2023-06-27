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
-- Module      : Amazonka.ApplicationInsights.UpdateLogPattern
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a log pattern to a @LogPatternSet@.
module Amazonka.ApplicationInsights.UpdateLogPattern
  ( -- * Creating a Request
    UpdateLogPattern (..),
    newUpdateLogPattern,

    -- * Request Lenses
    updateLogPattern_pattern,
    updateLogPattern_rank,
    updateLogPattern_resourceGroupName,
    updateLogPattern_patternSetName,
    updateLogPattern_patternName,

    -- * Destructuring the Response
    UpdateLogPatternResponse (..),
    newUpdateLogPatternResponse,

    -- * Response Lenses
    updateLogPatternResponse_logPattern,
    updateLogPatternResponse_resourceGroupName,
    updateLogPatternResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLogPattern' smart constructor.
data UpdateLogPattern = UpdateLogPattern'
  { -- | The log pattern. The pattern must be DFA compatible. Patterns that
    -- utilize forward lookahead or backreference constructions are not
    -- supported.
    pattern' :: Prelude.Maybe Prelude.Text,
    -- | Rank of the log pattern. Must be a value between @1@ and @1,000,000@.
    -- The patterns are sorted by rank, so we recommend that you set your
    -- highest priority patterns with the lowest rank. A pattern of rank @1@
    -- will be the first to get matched to a log line. A pattern of rank
    -- @1,000,000@ will be last to get matched. When you configure custom log
    -- patterns from the console, a @Low@ severity pattern translates to a
    -- @750,000@ rank. A @Medium@ severity pattern translates to a @500,000@
    -- rank. And a @High@ severity pattern translates to a @250,000@ rank. Rank
    -- values less than @1@ or greater than @1,000,000@ are reserved for
    -- AWS-provided patterns.
    rank :: Prelude.Maybe Prelude.Int,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the log pattern set.
    patternSetName :: Prelude.Text,
    -- | The name of the log pattern.
    patternName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLogPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pattern'', 'updateLogPattern_pattern' - The log pattern. The pattern must be DFA compatible. Patterns that
-- utilize forward lookahead or backreference constructions are not
-- supported.
--
-- 'rank', 'updateLogPattern_rank' - Rank of the log pattern. Must be a value between @1@ and @1,000,000@.
-- The patterns are sorted by rank, so we recommend that you set your
-- highest priority patterns with the lowest rank. A pattern of rank @1@
-- will be the first to get matched to a log line. A pattern of rank
-- @1,000,000@ will be last to get matched. When you configure custom log
-- patterns from the console, a @Low@ severity pattern translates to a
-- @750,000@ rank. A @Medium@ severity pattern translates to a @500,000@
-- rank. And a @High@ severity pattern translates to a @250,000@ rank. Rank
-- values less than @1@ or greater than @1,000,000@ are reserved for
-- AWS-provided patterns.
--
-- 'resourceGroupName', 'updateLogPattern_resourceGroupName' - The name of the resource group.
--
-- 'patternSetName', 'updateLogPattern_patternSetName' - The name of the log pattern set.
--
-- 'patternName', 'updateLogPattern_patternName' - The name of the log pattern.
newUpdateLogPattern ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'patternSetName'
  Prelude.Text ->
  -- | 'patternName'
  Prelude.Text ->
  UpdateLogPattern
newUpdateLogPattern
  pResourceGroupName_
  pPatternSetName_
  pPatternName_ =
    UpdateLogPattern'
      { pattern' = Prelude.Nothing,
        rank = Prelude.Nothing,
        resourceGroupName = pResourceGroupName_,
        patternSetName = pPatternSetName_,
        patternName = pPatternName_
      }

-- | The log pattern. The pattern must be DFA compatible. Patterns that
-- utilize forward lookahead or backreference constructions are not
-- supported.
updateLogPattern_pattern :: Lens.Lens' UpdateLogPattern (Prelude.Maybe Prelude.Text)
updateLogPattern_pattern = Lens.lens (\UpdateLogPattern' {pattern'} -> pattern') (\s@UpdateLogPattern' {} a -> s {pattern' = a} :: UpdateLogPattern)

-- | Rank of the log pattern. Must be a value between @1@ and @1,000,000@.
-- The patterns are sorted by rank, so we recommend that you set your
-- highest priority patterns with the lowest rank. A pattern of rank @1@
-- will be the first to get matched to a log line. A pattern of rank
-- @1,000,000@ will be last to get matched. When you configure custom log
-- patterns from the console, a @Low@ severity pattern translates to a
-- @750,000@ rank. A @Medium@ severity pattern translates to a @500,000@
-- rank. And a @High@ severity pattern translates to a @250,000@ rank. Rank
-- values less than @1@ or greater than @1,000,000@ are reserved for
-- AWS-provided patterns.
updateLogPattern_rank :: Lens.Lens' UpdateLogPattern (Prelude.Maybe Prelude.Int)
updateLogPattern_rank = Lens.lens (\UpdateLogPattern' {rank} -> rank) (\s@UpdateLogPattern' {} a -> s {rank = a} :: UpdateLogPattern)

-- | The name of the resource group.
updateLogPattern_resourceGroupName :: Lens.Lens' UpdateLogPattern Prelude.Text
updateLogPattern_resourceGroupName = Lens.lens (\UpdateLogPattern' {resourceGroupName} -> resourceGroupName) (\s@UpdateLogPattern' {} a -> s {resourceGroupName = a} :: UpdateLogPattern)

-- | The name of the log pattern set.
updateLogPattern_patternSetName :: Lens.Lens' UpdateLogPattern Prelude.Text
updateLogPattern_patternSetName = Lens.lens (\UpdateLogPattern' {patternSetName} -> patternSetName) (\s@UpdateLogPattern' {} a -> s {patternSetName = a} :: UpdateLogPattern)

-- | The name of the log pattern.
updateLogPattern_patternName :: Lens.Lens' UpdateLogPattern Prelude.Text
updateLogPattern_patternName = Lens.lens (\UpdateLogPattern' {patternName} -> patternName) (\s@UpdateLogPattern' {} a -> s {patternName = a} :: UpdateLogPattern)

instance Core.AWSRequest UpdateLogPattern where
  type
    AWSResponse UpdateLogPattern =
      UpdateLogPatternResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLogPatternResponse'
            Prelude.<$> (x Data..?> "LogPattern")
            Prelude.<*> (x Data..?> "ResourceGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLogPattern where
  hashWithSalt _salt UpdateLogPattern' {..} =
    _salt
      `Prelude.hashWithSalt` pattern'
      `Prelude.hashWithSalt` rank
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` patternSetName
      `Prelude.hashWithSalt` patternName

instance Prelude.NFData UpdateLogPattern where
  rnf UpdateLogPattern' {..} =
    Prelude.rnf pattern'
      `Prelude.seq` Prelude.rnf rank
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf patternSetName
      `Prelude.seq` Prelude.rnf patternName

instance Data.ToHeaders UpdateLogPattern where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.UpdateLogPattern" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLogPattern where
  toJSON UpdateLogPattern' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Pattern" Data..=) Prelude.<$> pattern',
            ("Rank" Data..=) Prelude.<$> rank,
            Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName),
            Prelude.Just
              ("PatternSetName" Data..= patternSetName),
            Prelude.Just ("PatternName" Data..= patternName)
          ]
      )

instance Data.ToPath UpdateLogPattern where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateLogPattern where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLogPatternResponse' smart constructor.
data UpdateLogPatternResponse = UpdateLogPatternResponse'
  { -- | The successfully created log pattern.
    logPattern :: Prelude.Maybe LogPattern,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLogPatternResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPattern', 'updateLogPatternResponse_logPattern' - The successfully created log pattern.
--
-- 'resourceGroupName', 'updateLogPatternResponse_resourceGroupName' - The name of the resource group.
--
-- 'httpStatus', 'updateLogPatternResponse_httpStatus' - The response's http status code.
newUpdateLogPatternResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLogPatternResponse
newUpdateLogPatternResponse pHttpStatus_ =
  UpdateLogPatternResponse'
    { logPattern =
        Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The successfully created log pattern.
updateLogPatternResponse_logPattern :: Lens.Lens' UpdateLogPatternResponse (Prelude.Maybe LogPattern)
updateLogPatternResponse_logPattern = Lens.lens (\UpdateLogPatternResponse' {logPattern} -> logPattern) (\s@UpdateLogPatternResponse' {} a -> s {logPattern = a} :: UpdateLogPatternResponse)

-- | The name of the resource group.
updateLogPatternResponse_resourceGroupName :: Lens.Lens' UpdateLogPatternResponse (Prelude.Maybe Prelude.Text)
updateLogPatternResponse_resourceGroupName = Lens.lens (\UpdateLogPatternResponse' {resourceGroupName} -> resourceGroupName) (\s@UpdateLogPatternResponse' {} a -> s {resourceGroupName = a} :: UpdateLogPatternResponse)

-- | The response's http status code.
updateLogPatternResponse_httpStatus :: Lens.Lens' UpdateLogPatternResponse Prelude.Int
updateLogPatternResponse_httpStatus = Lens.lens (\UpdateLogPatternResponse' {httpStatus} -> httpStatus) (\s@UpdateLogPatternResponse' {} a -> s {httpStatus = a} :: UpdateLogPatternResponse)

instance Prelude.NFData UpdateLogPatternResponse where
  rnf UpdateLogPatternResponse' {..} =
    Prelude.rnf logPattern
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf httpStatus
