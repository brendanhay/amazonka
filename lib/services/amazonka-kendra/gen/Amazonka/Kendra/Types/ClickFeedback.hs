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
-- Module      : Amazonka.Kendra.Types.ClickFeedback
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ClickFeedback where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Gathers information about when a particular result was clicked by a
-- user. Your application uses the @SubmitFeedback@ API to provide click
-- information.
--
-- /See:/ 'newClickFeedback' smart constructor.
data ClickFeedback = ClickFeedback'
  { -- | The identifier of the search result that was clicked.
    resultId :: Prelude.Text,
    -- | The Unix timestamp when the result was clicked.
    clickTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClickFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultId', 'clickFeedback_resultId' - The identifier of the search result that was clicked.
--
-- 'clickTime', 'clickFeedback_clickTime' - The Unix timestamp when the result was clicked.
newClickFeedback ::
  -- | 'resultId'
  Prelude.Text ->
  -- | 'clickTime'
  Prelude.UTCTime ->
  ClickFeedback
newClickFeedback pResultId_ pClickTime_ =
  ClickFeedback'
    { resultId = pResultId_,
      clickTime = Data._Time Lens.# pClickTime_
    }

-- | The identifier of the search result that was clicked.
clickFeedback_resultId :: Lens.Lens' ClickFeedback Prelude.Text
clickFeedback_resultId = Lens.lens (\ClickFeedback' {resultId} -> resultId) (\s@ClickFeedback' {} a -> s {resultId = a} :: ClickFeedback)

-- | The Unix timestamp when the result was clicked.
clickFeedback_clickTime :: Lens.Lens' ClickFeedback Prelude.UTCTime
clickFeedback_clickTime = Lens.lens (\ClickFeedback' {clickTime} -> clickTime) (\s@ClickFeedback' {} a -> s {clickTime = a} :: ClickFeedback) Prelude.. Data._Time

instance Prelude.Hashable ClickFeedback where
  hashWithSalt _salt ClickFeedback' {..} =
    _salt
      `Prelude.hashWithSalt` resultId
      `Prelude.hashWithSalt` clickTime

instance Prelude.NFData ClickFeedback where
  rnf ClickFeedback' {..} =
    Prelude.rnf resultId
      `Prelude.seq` Prelude.rnf clickTime

instance Data.ToJSON ClickFeedback where
  toJSON ClickFeedback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResultId" Data..= resultId),
            Prelude.Just ("ClickTime" Data..= clickTime)
          ]
      )
