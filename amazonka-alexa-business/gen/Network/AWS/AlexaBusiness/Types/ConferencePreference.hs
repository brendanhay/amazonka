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
-- Module      : Network.AWS.AlexaBusiness.Types.ConferencePreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferencePreference where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The default conference provider that is used if no other scheduled
-- meetings are detected.
--
-- /See:/ 'newConferencePreference' smart constructor.
data ConferencePreference = ConferencePreference'
  { -- | The ARN of the default conference provider.
    defaultConferenceProviderArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConferencePreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultConferenceProviderArn', 'conferencePreference_defaultConferenceProviderArn' - The ARN of the default conference provider.
newConferencePreference ::
  ConferencePreference
newConferencePreference =
  ConferencePreference'
    { defaultConferenceProviderArn =
        Core.Nothing
    }

-- | The ARN of the default conference provider.
conferencePreference_defaultConferenceProviderArn :: Lens.Lens' ConferencePreference (Core.Maybe Core.Text)
conferencePreference_defaultConferenceProviderArn = Lens.lens (\ConferencePreference' {defaultConferenceProviderArn} -> defaultConferenceProviderArn) (\s@ConferencePreference' {} a -> s {defaultConferenceProviderArn = a} :: ConferencePreference)

instance Core.FromJSON ConferencePreference where
  parseJSON =
    Core.withObject
      "ConferencePreference"
      ( \x ->
          ConferencePreference'
            Core.<$> (x Core..:? "DefaultConferenceProviderArn")
      )

instance Core.Hashable ConferencePreference

instance Core.NFData ConferencePreference

instance Core.ToJSON ConferencePreference where
  toJSON ConferencePreference' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultConferenceProviderArn" Core..=)
              Core.<$> defaultConferenceProviderArn
          ]
      )
