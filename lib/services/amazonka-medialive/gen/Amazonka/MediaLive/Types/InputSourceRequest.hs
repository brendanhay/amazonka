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
-- Module      : Amazonka.MediaLive.Types.InputSourceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for for a PULL type input.
--
-- /See:/ 'newInputSourceRequest' smart constructor.
data InputSourceRequest = InputSourceRequest'
  { -- | This represents the customer\'s source URL where stream is pulled from.
    url :: Prelude.Maybe Prelude.Text,
    -- | The username for the input source.
    username :: Prelude.Maybe Prelude.Text,
    -- | The key used to extract the password from EC2 Parameter store.
    passwordParam :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'inputSourceRequest_url' - This represents the customer\'s source URL where stream is pulled from.
--
-- 'username', 'inputSourceRequest_username' - The username for the input source.
--
-- 'passwordParam', 'inputSourceRequest_passwordParam' - The key used to extract the password from EC2 Parameter store.
newInputSourceRequest ::
  InputSourceRequest
newInputSourceRequest =
  InputSourceRequest'
    { url = Prelude.Nothing,
      username = Prelude.Nothing,
      passwordParam = Prelude.Nothing
    }

-- | This represents the customer\'s source URL where stream is pulled from.
inputSourceRequest_url :: Lens.Lens' InputSourceRequest (Prelude.Maybe Prelude.Text)
inputSourceRequest_url = Lens.lens (\InputSourceRequest' {url} -> url) (\s@InputSourceRequest' {} a -> s {url = a} :: InputSourceRequest)

-- | The username for the input source.
inputSourceRequest_username :: Lens.Lens' InputSourceRequest (Prelude.Maybe Prelude.Text)
inputSourceRequest_username = Lens.lens (\InputSourceRequest' {username} -> username) (\s@InputSourceRequest' {} a -> s {username = a} :: InputSourceRequest)

-- | The key used to extract the password from EC2 Parameter store.
inputSourceRequest_passwordParam :: Lens.Lens' InputSourceRequest (Prelude.Maybe Prelude.Text)
inputSourceRequest_passwordParam = Lens.lens (\InputSourceRequest' {passwordParam} -> passwordParam) (\s@InputSourceRequest' {} a -> s {passwordParam = a} :: InputSourceRequest)

instance Prelude.Hashable InputSourceRequest

instance Prelude.NFData InputSourceRequest

instance Core.ToJSON InputSourceRequest where
  toJSON InputSourceRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("url" Core..=) Prelude.<$> url,
            ("username" Core..=) Prelude.<$> username,
            ("passwordParam" Core..=) Prelude.<$> passwordParam
          ]
      )
