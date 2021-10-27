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
-- Module      : Network.AWS.Kendra.Types.ClickFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ClickFeedback where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Gathers information about when a particular result was clicked by a
-- user. Your application uses the @SubmitFeedback@ operation to provide
-- click information.
--
-- /See:/ 'newClickFeedback' smart constructor.
data ClickFeedback = ClickFeedback'
  { -- | The unique identifier of the search result that was clicked.
    resultId :: Prelude.Text,
    -- | The Unix timestamp of the date and time that the result was clicked.
    clickTime :: Core.POSIX
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
-- 'resultId', 'clickFeedback_resultId' - The unique identifier of the search result that was clicked.
--
-- 'clickTime', 'clickFeedback_clickTime' - The Unix timestamp of the date and time that the result was clicked.
newClickFeedback ::
  -- | 'resultId'
  Prelude.Text ->
  -- | 'clickTime'
  Prelude.UTCTime ->
  ClickFeedback
newClickFeedback pResultId_ pClickTime_ =
  ClickFeedback'
    { resultId = pResultId_,
      clickTime = Core._Time Lens.# pClickTime_
    }

-- | The unique identifier of the search result that was clicked.
clickFeedback_resultId :: Lens.Lens' ClickFeedback Prelude.Text
clickFeedback_resultId = Lens.lens (\ClickFeedback' {resultId} -> resultId) (\s@ClickFeedback' {} a -> s {resultId = a} :: ClickFeedback)

-- | The Unix timestamp of the date and time that the result was clicked.
clickFeedback_clickTime :: Lens.Lens' ClickFeedback Prelude.UTCTime
clickFeedback_clickTime = Lens.lens (\ClickFeedback' {clickTime} -> clickTime) (\s@ClickFeedback' {} a -> s {clickTime = a} :: ClickFeedback) Prelude.. Core._Time

instance Prelude.Hashable ClickFeedback

instance Prelude.NFData ClickFeedback

instance Core.ToJSON ClickFeedback where
  toJSON ClickFeedback' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResultId" Core..= resultId),
            Prelude.Just ("ClickTime" Core..= clickTime)
          ]
      )
