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
-- Module      : Amazonka.WorkLink.Types.WebsiteCaSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types.WebsiteCaSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of the certificate authority (CA).
--
-- /See:/ 'newWebsiteCaSummary' smart constructor.
data WebsiteCaSummary = WebsiteCaSummary'
  { -- | The time when the CA was added.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | A unique identifier for the CA.
    websiteCaId :: Prelude.Maybe Prelude.Text,
    -- | The name to display.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebsiteCaSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'websiteCaSummary_createdTime' - The time when the CA was added.
--
-- 'websiteCaId', 'websiteCaSummary_websiteCaId' - A unique identifier for the CA.
--
-- 'displayName', 'websiteCaSummary_displayName' - The name to display.
newWebsiteCaSummary ::
  WebsiteCaSummary
newWebsiteCaSummary =
  WebsiteCaSummary'
    { createdTime = Prelude.Nothing,
      websiteCaId = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The time when the CA was added.
websiteCaSummary_createdTime :: Lens.Lens' WebsiteCaSummary (Prelude.Maybe Prelude.UTCTime)
websiteCaSummary_createdTime = Lens.lens (\WebsiteCaSummary' {createdTime} -> createdTime) (\s@WebsiteCaSummary' {} a -> s {createdTime = a} :: WebsiteCaSummary) Prelude.. Lens.mapping Core._Time

-- | A unique identifier for the CA.
websiteCaSummary_websiteCaId :: Lens.Lens' WebsiteCaSummary (Prelude.Maybe Prelude.Text)
websiteCaSummary_websiteCaId = Lens.lens (\WebsiteCaSummary' {websiteCaId} -> websiteCaId) (\s@WebsiteCaSummary' {} a -> s {websiteCaId = a} :: WebsiteCaSummary)

-- | The name to display.
websiteCaSummary_displayName :: Lens.Lens' WebsiteCaSummary (Prelude.Maybe Prelude.Text)
websiteCaSummary_displayName = Lens.lens (\WebsiteCaSummary' {displayName} -> displayName) (\s@WebsiteCaSummary' {} a -> s {displayName = a} :: WebsiteCaSummary)

instance Core.FromJSON WebsiteCaSummary where
  parseJSON =
    Core.withObject
      "WebsiteCaSummary"
      ( \x ->
          WebsiteCaSummary'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "WebsiteCaId")
            Prelude.<*> (x Core..:? "DisplayName")
      )

instance Prelude.Hashable WebsiteCaSummary where
  hashWithSalt salt' WebsiteCaSummary' {..} =
    salt' `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` websiteCaId
      `Prelude.hashWithSalt` createdTime

instance Prelude.NFData WebsiteCaSummary where
  rnf WebsiteCaSummary' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf websiteCaId
