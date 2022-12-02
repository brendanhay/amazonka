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
-- Module      : Amazonka.MacieV2.Types.SessionContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SessionContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SessionContextAttributes
import Amazonka.MacieV2.Types.SessionIssuer
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a session that was created for an entity that
-- performed an action by using temporary security credentials.
--
-- /See:/ 'newSessionContext' smart constructor.
data SessionContext = SessionContext'
  { -- | The source and type of credentials that were issued to the entity.
    sessionIssuer :: Prelude.Maybe SessionIssuer,
    -- | The date and time when the credentials were issued, and whether the
    -- credentials were authenticated with a multi-factor authentication (MFA)
    -- device.
    attributes :: Prelude.Maybe SessionContextAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionIssuer', 'sessionContext_sessionIssuer' - The source and type of credentials that were issued to the entity.
--
-- 'attributes', 'sessionContext_attributes' - The date and time when the credentials were issued, and whether the
-- credentials were authenticated with a multi-factor authentication (MFA)
-- device.
newSessionContext ::
  SessionContext
newSessionContext =
  SessionContext'
    { sessionIssuer = Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | The source and type of credentials that were issued to the entity.
sessionContext_sessionIssuer :: Lens.Lens' SessionContext (Prelude.Maybe SessionIssuer)
sessionContext_sessionIssuer = Lens.lens (\SessionContext' {sessionIssuer} -> sessionIssuer) (\s@SessionContext' {} a -> s {sessionIssuer = a} :: SessionContext)

-- | The date and time when the credentials were issued, and whether the
-- credentials were authenticated with a multi-factor authentication (MFA)
-- device.
sessionContext_attributes :: Lens.Lens' SessionContext (Prelude.Maybe SessionContextAttributes)
sessionContext_attributes = Lens.lens (\SessionContext' {attributes} -> attributes) (\s@SessionContext' {} a -> s {attributes = a} :: SessionContext)

instance Data.FromJSON SessionContext where
  parseJSON =
    Data.withObject
      "SessionContext"
      ( \x ->
          SessionContext'
            Prelude.<$> (x Data..:? "sessionIssuer")
            Prelude.<*> (x Data..:? "attributes")
      )

instance Prelude.Hashable SessionContext where
  hashWithSalt _salt SessionContext' {..} =
    _salt `Prelude.hashWithSalt` sessionIssuer
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData SessionContext where
  rnf SessionContext' {..} =
    Prelude.rnf sessionIssuer
      `Prelude.seq` Prelude.rnf attributes
