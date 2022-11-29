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
-- Module      : Amazonka.AlexaBusiness.Types.ConferencePreference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.ConferencePreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The default conference provider that is used if no other scheduled
-- meetings are detected.
--
-- /See:/ 'newConferencePreference' smart constructor.
data ConferencePreference = ConferencePreference'
  { -- | The ARN of the default conference provider.
    defaultConferenceProviderArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ARN of the default conference provider.
conferencePreference_defaultConferenceProviderArn :: Lens.Lens' ConferencePreference (Prelude.Maybe Prelude.Text)
conferencePreference_defaultConferenceProviderArn = Lens.lens (\ConferencePreference' {defaultConferenceProviderArn} -> defaultConferenceProviderArn) (\s@ConferencePreference' {} a -> s {defaultConferenceProviderArn = a} :: ConferencePreference)

instance Core.FromJSON ConferencePreference where
  parseJSON =
    Core.withObject
      "ConferencePreference"
      ( \x ->
          ConferencePreference'
            Prelude.<$> (x Core..:? "DefaultConferenceProviderArn")
      )

instance Prelude.Hashable ConferencePreference where
  hashWithSalt _salt ConferencePreference' {..} =
    _salt
      `Prelude.hashWithSalt` defaultConferenceProviderArn

instance Prelude.NFData ConferencePreference where
  rnf ConferencePreference' {..} =
    Prelude.rnf defaultConferenceProviderArn

instance Core.ToJSON ConferencePreference where
  toJSON ConferencePreference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultConferenceProviderArn" Core..=)
              Prelude.<$> defaultConferenceProviderArn
          ]
      )
