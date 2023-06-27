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
-- Module      : Amazonka.WellArchitected.Types.ProfileShareSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileShareSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ShareStatus

-- | Summary of a profile share.
--
-- /See:/ 'newProfileShareSummary' smart constructor.
data ProfileShareSummary = ProfileShareSummary'
  { shareId :: Prelude.Maybe Prelude.Text,
    sharedWith :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    -- | Profile share invitation status message.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileShareSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareId', 'profileShareSummary_shareId' - Undocumented member.
--
-- 'sharedWith', 'profileShareSummary_sharedWith' - Undocumented member.
--
-- 'status', 'profileShareSummary_status' - Undocumented member.
--
-- 'statusMessage', 'profileShareSummary_statusMessage' - Profile share invitation status message.
newProfileShareSummary ::
  ProfileShareSummary
newProfileShareSummary =
  ProfileShareSummary'
    { shareId = Prelude.Nothing,
      sharedWith = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | Undocumented member.
profileShareSummary_shareId :: Lens.Lens' ProfileShareSummary (Prelude.Maybe Prelude.Text)
profileShareSummary_shareId = Lens.lens (\ProfileShareSummary' {shareId} -> shareId) (\s@ProfileShareSummary' {} a -> s {shareId = a} :: ProfileShareSummary)

-- | Undocumented member.
profileShareSummary_sharedWith :: Lens.Lens' ProfileShareSummary (Prelude.Maybe Prelude.Text)
profileShareSummary_sharedWith = Lens.lens (\ProfileShareSummary' {sharedWith} -> sharedWith) (\s@ProfileShareSummary' {} a -> s {sharedWith = a} :: ProfileShareSummary)

-- | Undocumented member.
profileShareSummary_status :: Lens.Lens' ProfileShareSummary (Prelude.Maybe ShareStatus)
profileShareSummary_status = Lens.lens (\ProfileShareSummary' {status} -> status) (\s@ProfileShareSummary' {} a -> s {status = a} :: ProfileShareSummary)

-- | Profile share invitation status message.
profileShareSummary_statusMessage :: Lens.Lens' ProfileShareSummary (Prelude.Maybe Prelude.Text)
profileShareSummary_statusMessage = Lens.lens (\ProfileShareSummary' {statusMessage} -> statusMessage) (\s@ProfileShareSummary' {} a -> s {statusMessage = a} :: ProfileShareSummary)

instance Data.FromJSON ProfileShareSummary where
  parseJSON =
    Data.withObject
      "ProfileShareSummary"
      ( \x ->
          ProfileShareSummary'
            Prelude.<$> (x Data..:? "ShareId")
            Prelude.<*> (x Data..:? "SharedWith")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable ProfileShareSummary where
  hashWithSalt _salt ProfileShareSummary' {..} =
    _salt
      `Prelude.hashWithSalt` shareId
      `Prelude.hashWithSalt` sharedWith
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ProfileShareSummary where
  rnf ProfileShareSummary' {..} =
    Prelude.rnf shareId
      `Prelude.seq` Prelude.rnf sharedWith
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
