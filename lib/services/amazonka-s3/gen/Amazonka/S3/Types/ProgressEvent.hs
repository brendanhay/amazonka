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
-- Module      : Amazonka.S3.Types.ProgressEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ProgressEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Progress

-- | This data type contains information about the progress event of an
-- operation.
--
-- /See:/ 'newProgressEvent' smart constructor.
data ProgressEvent = ProgressEvent'
  { -- | The Progress event details.
    details :: Prelude.Maybe Progress
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  ProgressEvent' {details = Prelude.Nothing}

-- | The Progress event details.
progressEvent_details :: Lens.Lens' ProgressEvent (Prelude.Maybe Progress)
progressEvent_details = Lens.lens (\ProgressEvent' {details} -> details) (\s@ProgressEvent' {} a -> s {details = a} :: ProgressEvent)

instance Data.FromXML ProgressEvent where
  parseXML x =
    ProgressEvent' Prelude.<$> (x Data..@? "Details")

instance Prelude.Hashable ProgressEvent where
  hashWithSalt _salt ProgressEvent' {..} =
    _salt `Prelude.hashWithSalt` details

instance Prelude.NFData ProgressEvent where
  rnf ProgressEvent' {..} = Prelude.rnf details
