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
-- Module      : Amazonka.WorkMail.Types.EwsAvailabilityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.EwsAvailabilityProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an EWS based availability provider. This is only used as input
-- to the service.
--
-- /See:/ 'newEwsAvailabilityProvider' smart constructor.
data EwsAvailabilityProvider = EwsAvailabilityProvider'
  { -- | The endpoint of the remote EWS server.
    ewsEndpoint :: Prelude.Text,
    -- | The username used to authenticate the remote EWS server.
    ewsUsername :: Prelude.Text,
    -- | The password used to authenticate the remote EWS server.
    ewsPassword :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EwsAvailabilityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ewsEndpoint', 'ewsAvailabilityProvider_ewsEndpoint' - The endpoint of the remote EWS server.
--
-- 'ewsUsername', 'ewsAvailabilityProvider_ewsUsername' - The username used to authenticate the remote EWS server.
--
-- 'ewsPassword', 'ewsAvailabilityProvider_ewsPassword' - The password used to authenticate the remote EWS server.
newEwsAvailabilityProvider ::
  -- | 'ewsEndpoint'
  Prelude.Text ->
  -- | 'ewsUsername'
  Prelude.Text ->
  -- | 'ewsPassword'
  Prelude.Text ->
  EwsAvailabilityProvider
newEwsAvailabilityProvider
  pEwsEndpoint_
  pEwsUsername_
  pEwsPassword_ =
    EwsAvailabilityProvider'
      { ewsEndpoint =
          pEwsEndpoint_,
        ewsUsername = pEwsUsername_,
        ewsPassword = Core._Sensitive Lens.# pEwsPassword_
      }

-- | The endpoint of the remote EWS server.
ewsAvailabilityProvider_ewsEndpoint :: Lens.Lens' EwsAvailabilityProvider Prelude.Text
ewsAvailabilityProvider_ewsEndpoint = Lens.lens (\EwsAvailabilityProvider' {ewsEndpoint} -> ewsEndpoint) (\s@EwsAvailabilityProvider' {} a -> s {ewsEndpoint = a} :: EwsAvailabilityProvider)

-- | The username used to authenticate the remote EWS server.
ewsAvailabilityProvider_ewsUsername :: Lens.Lens' EwsAvailabilityProvider Prelude.Text
ewsAvailabilityProvider_ewsUsername = Lens.lens (\EwsAvailabilityProvider' {ewsUsername} -> ewsUsername) (\s@EwsAvailabilityProvider' {} a -> s {ewsUsername = a} :: EwsAvailabilityProvider)

-- | The password used to authenticate the remote EWS server.
ewsAvailabilityProvider_ewsPassword :: Lens.Lens' EwsAvailabilityProvider Prelude.Text
ewsAvailabilityProvider_ewsPassword = Lens.lens (\EwsAvailabilityProvider' {ewsPassword} -> ewsPassword) (\s@EwsAvailabilityProvider' {} a -> s {ewsPassword = a} :: EwsAvailabilityProvider) Prelude.. Core._Sensitive

instance Prelude.Hashable EwsAvailabilityProvider where
  hashWithSalt _salt EwsAvailabilityProvider' {..} =
    _salt `Prelude.hashWithSalt` ewsEndpoint
      `Prelude.hashWithSalt` ewsUsername
      `Prelude.hashWithSalt` ewsPassword

instance Prelude.NFData EwsAvailabilityProvider where
  rnf EwsAvailabilityProvider' {..} =
    Prelude.rnf ewsEndpoint
      `Prelude.seq` Prelude.rnf ewsUsername
      `Prelude.seq` Prelude.rnf ewsPassword

instance Core.ToJSON EwsAvailabilityProvider where
  toJSON EwsAvailabilityProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EwsEndpoint" Core..= ewsEndpoint),
            Prelude.Just ("EwsUsername" Core..= ewsUsername),
            Prelude.Just ("EwsPassword" Core..= ewsPassword)
          ]
      )
