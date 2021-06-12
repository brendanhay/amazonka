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
-- Module      : Network.AWS.MediaLive.Types.InputSourceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for for a PULL type input.
--
-- /See:/ 'newInputSourceRequest' smart constructor.
data InputSourceRequest = InputSourceRequest'
  { -- | The key used to extract the password from EC2 Parameter store.
    passwordParam :: Core.Maybe Core.Text,
    -- | The username for the input source.
    username :: Core.Maybe Core.Text,
    -- | This represents the customer\'s source URL where stream is pulled from.
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordParam', 'inputSourceRequest_passwordParam' - The key used to extract the password from EC2 Parameter store.
--
-- 'username', 'inputSourceRequest_username' - The username for the input source.
--
-- 'url', 'inputSourceRequest_url' - This represents the customer\'s source URL where stream is pulled from.
newInputSourceRequest ::
  InputSourceRequest
newInputSourceRequest =
  InputSourceRequest'
    { passwordParam = Core.Nothing,
      username = Core.Nothing,
      url = Core.Nothing
    }

-- | The key used to extract the password from EC2 Parameter store.
inputSourceRequest_passwordParam :: Lens.Lens' InputSourceRequest (Core.Maybe Core.Text)
inputSourceRequest_passwordParam = Lens.lens (\InputSourceRequest' {passwordParam} -> passwordParam) (\s@InputSourceRequest' {} a -> s {passwordParam = a} :: InputSourceRequest)

-- | The username for the input source.
inputSourceRequest_username :: Lens.Lens' InputSourceRequest (Core.Maybe Core.Text)
inputSourceRequest_username = Lens.lens (\InputSourceRequest' {username} -> username) (\s@InputSourceRequest' {} a -> s {username = a} :: InputSourceRequest)

-- | This represents the customer\'s source URL where stream is pulled from.
inputSourceRequest_url :: Lens.Lens' InputSourceRequest (Core.Maybe Core.Text)
inputSourceRequest_url = Lens.lens (\InputSourceRequest' {url} -> url) (\s@InputSourceRequest' {} a -> s {url = a} :: InputSourceRequest)

instance Core.Hashable InputSourceRequest

instance Core.NFData InputSourceRequest

instance Core.ToJSON InputSourceRequest where
  toJSON InputSourceRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("passwordParam" Core..=) Core.<$> passwordParam,
            ("username" Core..=) Core.<$> username,
            ("url" Core..=) Core.<$> url
          ]
      )
