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
-- Module      : Amazonka.WellArchitected.Types.LensShareSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensShareSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ShareStatus

-- | A lens share summary return object.
--
-- /See:/ 'newLensShareSummary' smart constructor.
data LensShareSummary = LensShareSummary'
  { shareId :: Prelude.Maybe Prelude.Text,
    sharedWith :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    -- | Optional message to compliment the Status field.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensShareSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'lensShareSummary_shareId' - Undocumented member.
--
-- 'sharedWith', 'lensShareSummary_sharedWith' - Undocumented member.
--
-- 'status', 'lensShareSummary_status' - Undocumented member.
--
-- 'statusMessage', 'lensShareSummary_statusMessage' - Optional message to compliment the Status field.
newLensShareSummary ::
  LensShareSummary
newLensShareSummary =
  LensShareSummary'
    { shareId = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | Undocumented member.
lensShareSummary_shareId :: Lens.Lens' LensShareSummary (Prelude.Maybe Prelude.Text)
lensShareSummary_shareId = Lens.lens (\LensShareSummary' {shareId} -> shareId) (\s@LensShareSummary' {} a -> s {shareId = a} :: LensShareSummary)

-- | Undocumented member.
lensShareSummary_sharedWith :: Lens.Lens' LensShareSummary (Prelude.Maybe Prelude.Text)
lensShareSummary_sharedWith = Lens.lens (\LensShareSummary' {sharedWith} -> sharedWith) (\s@LensShareSummary' {} a -> s {sharedWith = a} :: LensShareSummary)

-- | Undocumented member.
lensShareSummary_status :: Lens.Lens' LensShareSummary (Prelude.Maybe ShareStatus)
lensShareSummary_status = Lens.lens (\LensShareSummary' {status} -> status) (\s@LensShareSummary' {} a -> s {status = a} :: LensShareSummary)

-- | Optional message to compliment the Status field.
lensShareSummary_statusMessage :: Lens.Lens' LensShareSummary (Prelude.Maybe Prelude.Text)
lensShareSummary_statusMessage = Lens.lens (\LensShareSummary' {statusMessage} -> statusMessage) (\s@LensShareSummary' {} a -> s {statusMessage = a} :: LensShareSummary)

instance Data.FromJSON LensShareSummary where
  parseJSON =
    Data.withObject
      "LensShareSummary"
      ( \x ->
          LensShareSummary'
            Prelude.<$> (x Data..:? "ShareId")
            Prelude.<*> (x Data..:? "SharedWith")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable LensShareSummary where
  hashWithSalt _salt LensShareSummary' {..} =
    _salt
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData LensShareSummary where
  rnf LensShareSummary' {..} =
    Prelude.rnf shareId `Prelude.seq`
      Prelude.rnf sharedWith `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf statusMessage
