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
-- Module      : Network.AWS.IoT.Types.HttpUrlDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpUrlDestinationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | HTTP URL destination configuration used by the topic rule\'s HTTP
-- action.
--
-- /See:/ 'newHttpUrlDestinationConfiguration' smart constructor.
data HttpUrlDestinationConfiguration = HttpUrlDestinationConfiguration'
  { -- | The URL AWS IoT uses to confirm ownership of or access to the topic rule
    -- destination URL.
    confirmationUrl :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpUrlDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confirmationUrl', 'httpUrlDestinationConfiguration_confirmationUrl' - The URL AWS IoT uses to confirm ownership of or access to the topic rule
-- destination URL.
newHttpUrlDestinationConfiguration ::
  -- | 'confirmationUrl'
  Core.Text ->
  HttpUrlDestinationConfiguration
newHttpUrlDestinationConfiguration pConfirmationUrl_ =
  HttpUrlDestinationConfiguration'
    { confirmationUrl =
        pConfirmationUrl_
    }

-- | The URL AWS IoT uses to confirm ownership of or access to the topic rule
-- destination URL.
httpUrlDestinationConfiguration_confirmationUrl :: Lens.Lens' HttpUrlDestinationConfiguration Core.Text
httpUrlDestinationConfiguration_confirmationUrl = Lens.lens (\HttpUrlDestinationConfiguration' {confirmationUrl} -> confirmationUrl) (\s@HttpUrlDestinationConfiguration' {} a -> s {confirmationUrl = a} :: HttpUrlDestinationConfiguration)

instance
  Core.Hashable
    HttpUrlDestinationConfiguration

instance Core.NFData HttpUrlDestinationConfiguration

instance Core.ToJSON HttpUrlDestinationConfiguration where
  toJSON HttpUrlDestinationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("confirmationUrl" Core..= confirmationUrl)
          ]
      )
