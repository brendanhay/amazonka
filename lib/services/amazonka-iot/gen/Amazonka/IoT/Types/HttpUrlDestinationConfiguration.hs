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
-- Module      : Amazonka.IoT.Types.HttpUrlDestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.HttpUrlDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | HTTP URL destination configuration used by the topic rule\'s HTTP
-- action.
--
-- /See:/ 'newHttpUrlDestinationConfiguration' smart constructor.
data HttpUrlDestinationConfiguration = HttpUrlDestinationConfiguration'
  { -- | The URL IoT uses to confirm ownership of or access to the topic rule
    -- destination URL.
    confirmationUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpUrlDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confirmationUrl', 'httpUrlDestinationConfiguration_confirmationUrl' - The URL IoT uses to confirm ownership of or access to the topic rule
-- destination URL.
newHttpUrlDestinationConfiguration ::
  -- | 'confirmationUrl'
  Prelude.Text ->
  HttpUrlDestinationConfiguration
newHttpUrlDestinationConfiguration pConfirmationUrl_ =
  HttpUrlDestinationConfiguration'
    { confirmationUrl =
        pConfirmationUrl_
    }

-- | The URL IoT uses to confirm ownership of or access to the topic rule
-- destination URL.
httpUrlDestinationConfiguration_confirmationUrl :: Lens.Lens' HttpUrlDestinationConfiguration Prelude.Text
httpUrlDestinationConfiguration_confirmationUrl = Lens.lens (\HttpUrlDestinationConfiguration' {confirmationUrl} -> confirmationUrl) (\s@HttpUrlDestinationConfiguration' {} a -> s {confirmationUrl = a} :: HttpUrlDestinationConfiguration)

instance
  Prelude.Hashable
    HttpUrlDestinationConfiguration
  where
  hashWithSalt
    _salt
    HttpUrlDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` confirmationUrl

instance
  Prelude.NFData
    HttpUrlDestinationConfiguration
  where
  rnf HttpUrlDestinationConfiguration' {..} =
    Prelude.rnf confirmationUrl

instance Data.ToJSON HttpUrlDestinationConfiguration where
  toJSON HttpUrlDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("confirmationUrl" Data..= confirmationUrl)
          ]
      )
