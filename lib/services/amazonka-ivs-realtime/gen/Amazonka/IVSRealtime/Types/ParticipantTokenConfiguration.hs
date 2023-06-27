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
-- Module      : Amazonka.IVSRealtime.Types.ParticipantTokenConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.ParticipantTokenConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types.ParticipantTokenCapability
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a participant token configuration in a stage.
--
-- /See:/ 'newParticipantTokenConfiguration' smart constructor.
data ParticipantTokenConfiguration = ParticipantTokenConfiguration'
  { -- | Application-provided attributes to encode into the corresponding
    -- participant token and attach to a stage. Map keys and values can contain
    -- UTF-8 encoded text. The maximum length of this field is 1 KB total.
    -- /This field is exposed to all stage participants and should not be used
    -- for personally identifying, confidential, or sensitive information./
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Set of capabilities that the user is allowed to perform in the stage.
    capabilities :: Prelude.Maybe [ParticipantTokenCapability],
    -- | Duration (in minutes), after which the corresponding participant token
    -- expires. Default: 720 (12 hours).
    duration :: Prelude.Maybe Prelude.Natural,
    -- | Customer-assigned name to help identify the token; this can be used to
    -- link a participant to a user in the customer’s own systems. This can be
    -- any UTF-8 encoded text. /This field is exposed to all stage participants
    -- and should not be used for personally identifying, confidential, or
    -- sensitive information./
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantTokenConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'participantTokenConfiguration_attributes' - Application-provided attributes to encode into the corresponding
-- participant token and attach to a stage. Map keys and values can contain
-- UTF-8 encoded text. The maximum length of this field is 1 KB total.
-- /This field is exposed to all stage participants and should not be used
-- for personally identifying, confidential, or sensitive information./
--
-- 'capabilities', 'participantTokenConfiguration_capabilities' - Set of capabilities that the user is allowed to perform in the stage.
--
-- 'duration', 'participantTokenConfiguration_duration' - Duration (in minutes), after which the corresponding participant token
-- expires. Default: 720 (12 hours).
--
-- 'userId', 'participantTokenConfiguration_userId' - Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information./
newParticipantTokenConfiguration ::
  ParticipantTokenConfiguration
newParticipantTokenConfiguration =
  ParticipantTokenConfiguration'
    { attributes =
        Prelude.Nothing,
      capabilities = Prelude.Nothing,
      duration = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | Application-provided attributes to encode into the corresponding
-- participant token and attach to a stage. Map keys and values can contain
-- UTF-8 encoded text. The maximum length of this field is 1 KB total.
-- /This field is exposed to all stage participants and should not be used
-- for personally identifying, confidential, or sensitive information./
participantTokenConfiguration_attributes :: Lens.Lens' ParticipantTokenConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
participantTokenConfiguration_attributes = Lens.lens (\ParticipantTokenConfiguration' {attributes} -> attributes) (\s@ParticipantTokenConfiguration' {} a -> s {attributes = a} :: ParticipantTokenConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Set of capabilities that the user is allowed to perform in the stage.
participantTokenConfiguration_capabilities :: Lens.Lens' ParticipantTokenConfiguration (Prelude.Maybe [ParticipantTokenCapability])
participantTokenConfiguration_capabilities = Lens.lens (\ParticipantTokenConfiguration' {capabilities} -> capabilities) (\s@ParticipantTokenConfiguration' {} a -> s {capabilities = a} :: ParticipantTokenConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Duration (in minutes), after which the corresponding participant token
-- expires. Default: 720 (12 hours).
participantTokenConfiguration_duration :: Lens.Lens' ParticipantTokenConfiguration (Prelude.Maybe Prelude.Natural)
participantTokenConfiguration_duration = Lens.lens (\ParticipantTokenConfiguration' {duration} -> duration) (\s@ParticipantTokenConfiguration' {} a -> s {duration = a} :: ParticipantTokenConfiguration)

-- | Customer-assigned name to help identify the token; this can be used to
-- link a participant to a user in the customer’s own systems. This can be
-- any UTF-8 encoded text. /This field is exposed to all stage participants
-- and should not be used for personally identifying, confidential, or
-- sensitive information./
participantTokenConfiguration_userId :: Lens.Lens' ParticipantTokenConfiguration (Prelude.Maybe Prelude.Text)
participantTokenConfiguration_userId = Lens.lens (\ParticipantTokenConfiguration' {userId} -> userId) (\s@ParticipantTokenConfiguration' {} a -> s {userId = a} :: ParticipantTokenConfiguration)

instance
  Prelude.Hashable
    ParticipantTokenConfiguration
  where
  hashWithSalt _salt ParticipantTokenConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` userId

instance Prelude.NFData ParticipantTokenConfiguration where
  rnf ParticipantTokenConfiguration' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf userId

instance Data.ToJSON ParticipantTokenConfiguration where
  toJSON ParticipantTokenConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("capabilities" Data..=) Prelude.<$> capabilities,
            ("duration" Data..=) Prelude.<$> duration,
            ("userId" Data..=) Prelude.<$> userId
          ]
      )
