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
-- Module      : Amazonka.DataExchange.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.AutoExportRevisionToS3RequestDetails
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "ExportRevisionToS3")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt `Prelude.hashWithSalt` exportRevisionToS3

instance Prelude.NFData Action where
  rnf Action' {..} = Prelude.rnf exportRevisionToS3

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExportRevisionToS3" Data..=)
              Prelude.<$> exportRevisionToS3
          ]
      )
