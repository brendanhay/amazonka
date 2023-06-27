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
-- Module      : Amazonka.MediaLive.Types.InputSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for a PULL type input.
--
-- /See:/ 'newInputSource' smart constructor.
data InputSource = InputSource'
  { -- | The key used to extract the password from EC2 Parameter store.
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | This represents the customer\'s source URL where stream is pulled from.
    url :: Prelude.Maybe Prelude.Text,
    -- | The username for the input source.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'url', 'inputSource_url' - This represents the customer\'s source URL where stream is pulled from.
--
-- 'username', 'inputSource_username' - The username for the input source.
newInputSource ::
  InputSource
newInputSource =
  InputSource'
    { passwordParam = Prelude.Nothing,
      url = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The key used to extract the password from EC2 Parameter store.
inputSource_passwordParam :: Lens.Lens' InputSource (Prelude.Maybe Prelude.Text)
inputSource_passwordParam = Lens.lens (\InputSource' {passwordParam} -> passwordParam) (\s@InputSource' {} a -> s {passwordParam = a} :: InputSource)

-- | This represents the customer\'s source URL where stream is pulled from.
inputSource_url :: Lens.Lens' InputSource (Prelude.Maybe Prelude.Text)
inputSource_url = Lens.lens (\InputSource' {url} -> url) (\s@InputSource' {} a -> s {url = a} :: InputSource)

-- | The username for the input source.
inputSource_username :: Lens.Lens' InputSource (Prelude.Maybe Prelude.Text)
inputSource_username = Lens.lens (\InputSource' {username} -> username) (\s@InputSource' {} a -> s {username = a} :: InputSource)

instance Data.FromJSON InputSource where
  parseJSON =
    Data.withObject
      "InputSource"
      ( \x ->
          InputSource'
            Prelude.<$> (x Data..:? "passwordParam")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "username")
      )

instance Prelude.Hashable InputSource where
  hashWithSalt _salt InputSource' {..} =
    _salt
      `Prelude.hashWithSalt` passwordParam
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` username

instance Prelude.NFData InputSource where
  rnf InputSource' {..} =
    Prelude.rnf passwordParam
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf username
