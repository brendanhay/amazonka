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
-- Module      : Network.AWS.DataExchange.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types.Action where

import qualified Network.AWS.Core as Core
import Network.AWS.DataExchange.Types.AutoExportRevisionToS3RequestDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | What occurs after a certain event.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Details for the export revision to Amazon S3 action.
    exportRevisionToS3 :: Prelude.Maybe AutoExportRevisionToS3RequestDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportRevisionToS3', 'action_exportRevisionToS3' - Details for the export revision to Amazon S3 action.
newAction ::
  Action
newAction =
  Action' {exportRevisionToS3 = Prelude.Nothing}

-- | Details for the export revision to Amazon S3 action.
action_exportRevisionToS3 :: Lens.Lens' Action (Prelude.Maybe AutoExportRevisionToS3RequestDetails)
action_exportRevisionToS3 = Lens.lens (\Action' {exportRevisionToS3} -> exportRevisionToS3) (\s@Action' {} a -> s {exportRevisionToS3 = a} :: Action)

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Core..:? "ExportRevisionToS3")
      )

instance Prelude.Hashable Action

instance Prelude.NFData Action

instance Core.ToJSON Action where
  toJSON Action' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExportRevisionToS3" Core..=)
              Prelude.<$> exportRevisionToS3
          ]
      )
