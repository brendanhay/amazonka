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
-- Module      : Network.AWS.S3.Types.ProgressEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ProgressEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Progress

-- | This data type contains information about the progress event of an
-- operation.
--
-- /See:/ 'newProgressEvent' smart constructor.
data ProgressEvent = ProgressEvent'
  { -- | The Progress event details.
    details :: Core.Maybe Progress
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProgressEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'progressEvent_details' - The Progress event details.
newProgressEvent ::
  ProgressEvent
newProgressEvent =
  ProgressEvent' {details = Core.Nothing}

-- | The Progress event details.
progressEvent_details :: Lens.Lens' ProgressEvent (Core.Maybe Progress)
progressEvent_details = Lens.lens (\ProgressEvent' {details} -> details) (\s@ProgressEvent' {} a -> s {details = a} :: ProgressEvent)

instance Core.FromXML ProgressEvent where
  parseXML x =
    ProgressEvent' Core.<$> (x Core..@? "Details")

instance Core.Hashable ProgressEvent

instance Core.NFData ProgressEvent
