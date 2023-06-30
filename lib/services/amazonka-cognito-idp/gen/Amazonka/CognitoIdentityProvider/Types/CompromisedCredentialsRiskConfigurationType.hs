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
-- Module      : Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType where

import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Amazonka.CognitoIdentityProvider.Types.EventFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The compromised credentials risk configuration type.
--
-- /See:/ 'newCompromisedCredentialsRiskConfigurationType' smart constructor.
data CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType'
  { -- | Perform the action for these events. The default is to perform all
    -- events if no event filter is specified.
    eventFilter :: Prelude.Maybe [EventFilterType],
    -- | The compromised credentials risk configuration actions.
    actions :: CompromisedCredentialsActionsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompromisedCredentialsRiskConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventFilter', 'compromisedCredentialsRiskConfigurationType_eventFilter' - Perform the action for these events. The default is to perform all
-- events if no event filter is specified.
--
-- 'actions', 'compromisedCredentialsRiskConfigurationType_actions' - The compromised credentials risk configuration actions.
newCompromisedCredentialsRiskConfigurationType ::
  -- | 'actions'
  CompromisedCredentialsActionsType ->
  CompromisedCredentialsRiskConfigurationType
newCompromisedCredentialsRiskConfigurationType
  pActions_ =
    CompromisedCredentialsRiskConfigurationType'
      { eventFilter =
          Prelude.Nothing,
        actions = pActions_
      }

-- | Perform the action for these events. The default is to perform all
-- events if no event filter is specified.
compromisedCredentialsRiskConfigurationType_eventFilter :: Lens.Lens' CompromisedCredentialsRiskConfigurationType (Prelude.Maybe [EventFilterType])
compromisedCredentialsRiskConfigurationType_eventFilter = Lens.lens (\CompromisedCredentialsRiskConfigurationType' {eventFilter} -> eventFilter) (\s@CompromisedCredentialsRiskConfigurationType' {} a -> s {eventFilter = a} :: CompromisedCredentialsRiskConfigurationType) Prelude.. Lens.mapping Lens.coerced

-- | The compromised credentials risk configuration actions.
compromisedCredentialsRiskConfigurationType_actions :: Lens.Lens' CompromisedCredentialsRiskConfigurationType CompromisedCredentialsActionsType
compromisedCredentialsRiskConfigurationType_actions = Lens.lens (\CompromisedCredentialsRiskConfigurationType' {actions} -> actions) (\s@CompromisedCredentialsRiskConfigurationType' {} a -> s {actions = a} :: CompromisedCredentialsRiskConfigurationType)

instance
  Data.FromJSON
    CompromisedCredentialsRiskConfigurationType
  where
  parseJSON =
    Data.withObject
      "CompromisedCredentialsRiskConfigurationType"
      ( \x ->
          CompromisedCredentialsRiskConfigurationType'
            Prelude.<$> (x Data..:? "EventFilter" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Actions")
      )

instance
  Prelude.Hashable
    CompromisedCredentialsRiskConfigurationType
  where
  hashWithSalt
    _salt
    CompromisedCredentialsRiskConfigurationType' {..} =
      _salt
        `Prelude.hashWithSalt` eventFilter
        `Prelude.hashWithSalt` actions

instance
  Prelude.NFData
    CompromisedCredentialsRiskConfigurationType
  where
  rnf CompromisedCredentialsRiskConfigurationType' {..} =
    Prelude.rnf eventFilter
      `Prelude.seq` Prelude.rnf actions

instance
  Data.ToJSON
    CompromisedCredentialsRiskConfigurationType
  where
  toJSON
    CompromisedCredentialsRiskConfigurationType' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("EventFilter" Data..=) Prelude.<$> eventFilter,
              Prelude.Just ("Actions" Data..= actions)
            ]
        )
