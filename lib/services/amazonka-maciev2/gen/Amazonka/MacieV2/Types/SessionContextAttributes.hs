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
-- Module      : Amazonka.MacieV2.Types.SessionContextAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SessionContextAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the context in which temporary security
-- credentials were issued to an entity.
--
-- /See:/ 'newSessionContextAttributes' smart constructor.
data SessionContextAttributes = SessionContextAttributes'
  { -- | Specifies whether the credentials were authenticated with a multi-factor
    -- authentication (MFA) device.
    mfaAuthenticated :: Prelude.Maybe Prelude.Bool,
    -- | The date and time, in UTC and ISO 8601 format, when the credentials were
    -- issued.
    creationDate :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionContextAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaAuthenticated', 'sessionContextAttributes_mfaAuthenticated' - Specifies whether the credentials were authenticated with a multi-factor
-- authentication (MFA) device.
--
-- 'creationDate', 'sessionContextAttributes_creationDate' - The date and time, in UTC and ISO 8601 format, when the credentials were
-- issued.
newSessionContextAttributes ::
  SessionContextAttributes
newSessionContextAttributes =
  SessionContextAttributes'
    { mfaAuthenticated =
        Prelude.Nothing,
      creationDate = Prelude.Nothing
    }

-- | Specifies whether the credentials were authenticated with a multi-factor
-- authentication (MFA) device.
sessionContextAttributes_mfaAuthenticated :: Lens.Lens' SessionContextAttributes (Prelude.Maybe Prelude.Bool)
sessionContextAttributes_mfaAuthenticated = Lens.lens (\SessionContextAttributes' {mfaAuthenticated} -> mfaAuthenticated) (\s@SessionContextAttributes' {} a -> s {mfaAuthenticated = a} :: SessionContextAttributes)

-- | The date and time, in UTC and ISO 8601 format, when the credentials were
-- issued.
sessionContextAttributes_creationDate :: Lens.Lens' SessionContextAttributes (Prelude.Maybe Prelude.UTCTime)
sessionContextAttributes_creationDate = Lens.lens (\SessionContextAttributes' {creationDate} -> creationDate) (\s@SessionContextAttributes' {} a -> s {creationDate = a} :: SessionContextAttributes) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SessionContextAttributes where
  parseJSON =
    Data.withObject
      "SessionContextAttributes"
      ( \x ->
          SessionContextAttributes'
            Prelude.<$> (x Data..:? "mfaAuthenticated")
            Prelude.<*> (x Data..:? "creationDate")
      )

instance Prelude.Hashable SessionContextAttributes where
  hashWithSalt _salt SessionContextAttributes' {..} =
    _salt `Prelude.hashWithSalt` mfaAuthenticated
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData SessionContextAttributes where
  rnf SessionContextAttributes' {..} =
    Prelude.rnf mfaAuthenticated
      `Prelude.seq` Prelude.rnf creationDate
