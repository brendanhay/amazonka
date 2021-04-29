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
-- Module      : Network.AWS.MediaLive.Types.InputSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The settings for a PULL type input.
--
-- /See:/ 'newInputSource' smart constructor.
data InputSource = InputSource'
  { -- | The key used to extract the password from EC2 Parameter store.
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | The username for the input source.
    username :: Prelude.Maybe Prelude.Text,
    -- | This represents the customer\'s source URL where stream is pulled from.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordParam', 'inputSource_passwordParam' - The key used to extract the password from EC2 Parameter store.
--
-- 'username', 'inputSource_username' - The username for the input source.
--
-- 'url', 'inputSource_url' - This represents the customer\'s source URL where stream is pulled from.
newInputSource ::
  InputSource
newInputSource =
  InputSource'
    { passwordParam = Prelude.Nothing,
      username = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The key used to extract the password from EC2 Parameter store.
inputSource_passwordParam :: Lens.Lens' InputSource (Prelude.Maybe Prelude.Text)
inputSource_passwordParam = Lens.lens (\InputSource' {passwordParam} -> passwordParam) (\s@InputSource' {} a -> s {passwordParam = a} :: InputSource)

-- | The username for the input source.
inputSource_username :: Lens.Lens' InputSource (Prelude.Maybe Prelude.Text)
inputSource_username = Lens.lens (\InputSource' {username} -> username) (\s@InputSource' {} a -> s {username = a} :: InputSource)

-- | This represents the customer\'s source URL where stream is pulled from.
inputSource_url :: Lens.Lens' InputSource (Prelude.Maybe Prelude.Text)
inputSource_url = Lens.lens (\InputSource' {url} -> url) (\s@InputSource' {} a -> s {url = a} :: InputSource)

instance Prelude.FromJSON InputSource where
  parseJSON =
    Prelude.withObject
      "InputSource"
      ( \x ->
          InputSource'
            Prelude.<$> (x Prelude..:? "passwordParam")
            Prelude.<*> (x Prelude..:? "username")
            Prelude.<*> (x Prelude..:? "url")
      )

instance Prelude.Hashable InputSource

instance Prelude.NFData InputSource
