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
-- Module      : Amazonka.VoiceId.Types.AuthenticationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.AuthenticationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration used to authenticate a speaker during a session.
--
-- /See:/ 'newAuthenticationConfiguration' smart constructor.
data AuthenticationConfiguration = AuthenticationConfiguration'
  { -- | The minimum threshold needed to successfully authenticate a speaker.
    acceptanceThreshold :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptanceThreshold', 'authenticationConfiguration_acceptanceThreshold' - The minimum threshold needed to successfully authenticate a speaker.
newAuthenticationConfiguration ::
  -- | 'acceptanceThreshold'
  Prelude.Natural ->
  AuthenticationConfiguration
newAuthenticationConfiguration pAcceptanceThreshold_ =
  AuthenticationConfiguration'
    { acceptanceThreshold =
        pAcceptanceThreshold_
    }

-- | The minimum threshold needed to successfully authenticate a speaker.
authenticationConfiguration_acceptanceThreshold :: Lens.Lens' AuthenticationConfiguration Prelude.Natural
authenticationConfiguration_acceptanceThreshold = Lens.lens (\AuthenticationConfiguration' {acceptanceThreshold} -> acceptanceThreshold) (\s@AuthenticationConfiguration' {} a -> s {acceptanceThreshold = a} :: AuthenticationConfiguration)

instance Data.FromJSON AuthenticationConfiguration where
  parseJSON =
    Data.withObject
      "AuthenticationConfiguration"
      ( \x ->
          AuthenticationConfiguration'
            Prelude.<$> (x Data..: "AcceptanceThreshold")
      )

instance Prelude.Hashable AuthenticationConfiguration where
  hashWithSalt _salt AuthenticationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` acceptanceThreshold

instance Prelude.NFData AuthenticationConfiguration where
  rnf AuthenticationConfiguration' {..} =
    Prelude.rnf acceptanceThreshold
