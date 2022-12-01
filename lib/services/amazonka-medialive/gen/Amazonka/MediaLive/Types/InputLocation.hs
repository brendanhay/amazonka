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
-- Module      : Amazonka.MediaLive.Types.InputLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Input Location
--
-- /See:/ 'newInputLocation' smart constructor.
data InputLocation = InputLocation'
  { -- | Documentation update needed
    username :: Prelude.Maybe Prelude.Text,
    -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | Uniform Resource Identifier - This should be a path to a file accessible
    -- to the Live system (eg. a http:\/\/ URI) depending on the output type.
    -- For example, a RTMP destination should have a uri simliar to:
    -- \"rtmp:\/\/fmsserver\/live\".
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'inputLocation_username' - Documentation update needed
--
-- 'passwordParam', 'inputLocation_passwordParam' - key used to extract the password from EC2 Parameter store
--
-- 'uri', 'inputLocation_uri' - Uniform Resource Identifier - This should be a path to a file accessible
-- to the Live system (eg. a http:\/\/ URI) depending on the output type.
-- For example, a RTMP destination should have a uri simliar to:
-- \"rtmp:\/\/fmsserver\/live\".
newInputLocation ::
  -- | 'uri'
  Prelude.Text ->
  InputLocation
newInputLocation pUri_ =
  InputLocation'
    { username = Prelude.Nothing,
      passwordParam = Prelude.Nothing,
      uri = pUri_
    }

-- | Documentation update needed
inputLocation_username :: Lens.Lens' InputLocation (Prelude.Maybe Prelude.Text)
inputLocation_username = Lens.lens (\InputLocation' {username} -> username) (\s@InputLocation' {} a -> s {username = a} :: InputLocation)

-- | key used to extract the password from EC2 Parameter store
inputLocation_passwordParam :: Lens.Lens' InputLocation (Prelude.Maybe Prelude.Text)
inputLocation_passwordParam = Lens.lens (\InputLocation' {passwordParam} -> passwordParam) (\s@InputLocation' {} a -> s {passwordParam = a} :: InputLocation)

-- | Uniform Resource Identifier - This should be a path to a file accessible
-- to the Live system (eg. a http:\/\/ URI) depending on the output type.
-- For example, a RTMP destination should have a uri simliar to:
-- \"rtmp:\/\/fmsserver\/live\".
inputLocation_uri :: Lens.Lens' InputLocation Prelude.Text
inputLocation_uri = Lens.lens (\InputLocation' {uri} -> uri) (\s@InputLocation' {} a -> s {uri = a} :: InputLocation)

instance Core.FromJSON InputLocation where
  parseJSON =
    Core.withObject
      "InputLocation"
      ( \x ->
          InputLocation'
            Prelude.<$> (x Core..:? "username")
            Prelude.<*> (x Core..:? "passwordParam")
            Prelude.<*> (x Core..: "uri")
      )

instance Prelude.Hashable InputLocation where
  hashWithSalt _salt InputLocation' {..} =
    _salt `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` passwordParam
      `Prelude.hashWithSalt` uri

instance Prelude.NFData InputLocation where
  rnf InputLocation' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf passwordParam
      `Prelude.seq` Prelude.rnf uri

instance Core.ToJSON InputLocation where
  toJSON InputLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("username" Core..=) Prelude.<$> username,
            ("passwordParam" Core..=) Prelude.<$> passwordParam,
            Prelude.Just ("uri" Core..= uri)
          ]
      )
