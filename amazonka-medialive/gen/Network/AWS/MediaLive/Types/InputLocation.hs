{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.InputLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Input Location
--
-- /See:/ 'newInputLocation' smart constructor.
data InputLocation = InputLocation'
  { -- | key used to extract the password from EC2 Parameter store
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | Documentation update needed
    username :: Prelude.Maybe Prelude.Text,
    -- | Uniform Resource Identifier - This should be a path to a file accessible
    -- to the Live system (eg. a http:\/\/ URI) depending on the output type.
    -- For example, a RTMP destination should have a uri simliar to:
    -- \"rtmp:\/\/fmsserver\/live\".
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordParam', 'inputLocation_passwordParam' - key used to extract the password from EC2 Parameter store
--
-- 'username', 'inputLocation_username' - Documentation update needed
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
    { passwordParam = Prelude.Nothing,
      username = Prelude.Nothing,
      uri = pUri_
    }

-- | key used to extract the password from EC2 Parameter store
inputLocation_passwordParam :: Lens.Lens' InputLocation (Prelude.Maybe Prelude.Text)
inputLocation_passwordParam = Lens.lens (\InputLocation' {passwordParam} -> passwordParam) (\s@InputLocation' {} a -> s {passwordParam = a} :: InputLocation)

-- | Documentation update needed
inputLocation_username :: Lens.Lens' InputLocation (Prelude.Maybe Prelude.Text)
inputLocation_username = Lens.lens (\InputLocation' {username} -> username) (\s@InputLocation' {} a -> s {username = a} :: InputLocation)

-- | Uniform Resource Identifier - This should be a path to a file accessible
-- to the Live system (eg. a http:\/\/ URI) depending on the output type.
-- For example, a RTMP destination should have a uri simliar to:
-- \"rtmp:\/\/fmsserver\/live\".
inputLocation_uri :: Lens.Lens' InputLocation Prelude.Text
inputLocation_uri = Lens.lens (\InputLocation' {uri} -> uri) (\s@InputLocation' {} a -> s {uri = a} :: InputLocation)

instance Prelude.FromJSON InputLocation where
  parseJSON =
    Prelude.withObject
      "InputLocation"
      ( \x ->
          InputLocation'
            Prelude.<$> (x Prelude..:? "passwordParam")
            Prelude.<*> (x Prelude..:? "username")
            Prelude.<*> (x Prelude..: "uri")
      )

instance Prelude.Hashable InputLocation

instance Prelude.NFData InputLocation

instance Prelude.ToJSON InputLocation where
  toJSON InputLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("passwordParam" Prelude..=)
              Prelude.<$> passwordParam,
            ("username" Prelude..=) Prelude.<$> username,
            Prelude.Just ("uri" Prelude..= uri)
          ]
      )
