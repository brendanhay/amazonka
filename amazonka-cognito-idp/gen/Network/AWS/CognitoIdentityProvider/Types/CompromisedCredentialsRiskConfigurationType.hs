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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Network.AWS.CognitoIdentityProvider.Types.EventFilterType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The compromised credentials risk configuration type.
--
-- /See:/ 'newCompromisedCredentialsRiskConfigurationType' smart constructor.
data CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType'
  { -- | Perform the action for these events. The default is to perform all
    -- events if no event filter is specified.
    eventFilter :: Core.Maybe [EventFilterType],
    -- | The compromised credentials risk configuration actions.
    actions :: CompromisedCredentialsActionsType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
          Core.Nothing,
        actions = pActions_
      }

-- | Perform the action for these events. The default is to perform all
-- events if no event filter is specified.
compromisedCredentialsRiskConfigurationType_eventFilter :: Lens.Lens' CompromisedCredentialsRiskConfigurationType (Core.Maybe [EventFilterType])
compromisedCredentialsRiskConfigurationType_eventFilter = Lens.lens (\CompromisedCredentialsRiskConfigurationType' {eventFilter} -> eventFilter) (\s@CompromisedCredentialsRiskConfigurationType' {} a -> s {eventFilter = a} :: CompromisedCredentialsRiskConfigurationType) Core.. Lens.mapping Lens._Coerce

-- | The compromised credentials risk configuration actions.
compromisedCredentialsRiskConfigurationType_actions :: Lens.Lens' CompromisedCredentialsRiskConfigurationType CompromisedCredentialsActionsType
compromisedCredentialsRiskConfigurationType_actions = Lens.lens (\CompromisedCredentialsRiskConfigurationType' {actions} -> actions) (\s@CompromisedCredentialsRiskConfigurationType' {} a -> s {actions = a} :: CompromisedCredentialsRiskConfigurationType)

instance
  Core.FromJSON
    CompromisedCredentialsRiskConfigurationType
  where
  parseJSON =
    Core.withObject
      "CompromisedCredentialsRiskConfigurationType"
      ( \x ->
          CompromisedCredentialsRiskConfigurationType'
            Core.<$> (x Core..:? "EventFilter" Core..!= Core.mempty)
            Core.<*> (x Core..: "Actions")
      )

instance
  Core.Hashable
    CompromisedCredentialsRiskConfigurationType

instance
  Core.NFData
    CompromisedCredentialsRiskConfigurationType

instance
  Core.ToJSON
    CompromisedCredentialsRiskConfigurationType
  where
  toJSON
    CompromisedCredentialsRiskConfigurationType' {..} =
      Core.object
        ( Core.catMaybes
            [ ("EventFilter" Core..=) Core.<$> eventFilter,
              Core.Just ("Actions" Core..= actions)
            ]
        )
