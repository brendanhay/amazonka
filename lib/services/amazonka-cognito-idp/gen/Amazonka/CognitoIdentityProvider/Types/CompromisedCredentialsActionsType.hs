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
-- Module      : Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType where

import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The compromised credentials actions type.
--
-- /See:/ 'newCompromisedCredentialsActionsType' smart constructor.
data CompromisedCredentialsActionsType = CompromisedCredentialsActionsType'
  { -- | The event action.
    eventAction :: CompromisedCredentialsEventActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompromisedCredentialsActionsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventAction', 'compromisedCredentialsActionsType_eventAction' - The event action.
newCompromisedCredentialsActionsType ::
  -- | 'eventAction'
  CompromisedCredentialsEventActionType ->
  CompromisedCredentialsActionsType
newCompromisedCredentialsActionsType pEventAction_ =
  CompromisedCredentialsActionsType'
    { eventAction =
        pEventAction_
    }

-- | The event action.
compromisedCredentialsActionsType_eventAction :: Lens.Lens' CompromisedCredentialsActionsType CompromisedCredentialsEventActionType
compromisedCredentialsActionsType_eventAction = Lens.lens (\CompromisedCredentialsActionsType' {eventAction} -> eventAction) (\s@CompromisedCredentialsActionsType' {} a -> s {eventAction = a} :: CompromisedCredentialsActionsType)

instance
  Core.FromJSON
    CompromisedCredentialsActionsType
  where
  parseJSON =
    Core.withObject
      "CompromisedCredentialsActionsType"
      ( \x ->
          CompromisedCredentialsActionsType'
            Prelude.<$> (x Core..: "EventAction")
      )

instance
  Prelude.Hashable
    CompromisedCredentialsActionsType
  where
  hashWithSalt
    _salt
    CompromisedCredentialsActionsType' {..} =
      _salt `Prelude.hashWithSalt` eventAction

instance
  Prelude.NFData
    CompromisedCredentialsActionsType
  where
  rnf CompromisedCredentialsActionsType' {..} =
    Prelude.rnf eventAction

instance
  Core.ToJSON
    CompromisedCredentialsActionsType
  where
  toJSON CompromisedCredentialsActionsType' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("EventAction" Core..= eventAction)]
      )
