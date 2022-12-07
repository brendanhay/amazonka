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
-- Module      : Amazonka.IoT.Types.HttpUrlDestinationProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.HttpUrlDestinationProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | HTTP URL destination properties.
--
-- /See:/ 'newHttpUrlDestinationProperties' smart constructor.
data HttpUrlDestinationProperties = HttpUrlDestinationProperties'
  { -- | The URL used to confirm the HTTP topic rule destination URL.
    confirmationUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpUrlDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confirmationUrl', 'httpUrlDestinationProperties_confirmationUrl' - The URL used to confirm the HTTP topic rule destination URL.
newHttpUrlDestinationProperties ::
  HttpUrlDestinationProperties
newHttpUrlDestinationProperties =
  HttpUrlDestinationProperties'
    { confirmationUrl =
        Prelude.Nothing
    }

-- | The URL used to confirm the HTTP topic rule destination URL.
httpUrlDestinationProperties_confirmationUrl :: Lens.Lens' HttpUrlDestinationProperties (Prelude.Maybe Prelude.Text)
httpUrlDestinationProperties_confirmationUrl = Lens.lens (\HttpUrlDestinationProperties' {confirmationUrl} -> confirmationUrl) (\s@HttpUrlDestinationProperties' {} a -> s {confirmationUrl = a} :: HttpUrlDestinationProperties)

instance Data.FromJSON HttpUrlDestinationProperties where
  parseJSON =
    Data.withObject
      "HttpUrlDestinationProperties"
      ( \x ->
          HttpUrlDestinationProperties'
            Prelude.<$> (x Data..:? "confirmationUrl")
      )

instance
  Prelude.Hashable
    HttpUrlDestinationProperties
  where
  hashWithSalt _salt HttpUrlDestinationProperties' {..} =
    _salt `Prelude.hashWithSalt` confirmationUrl

instance Prelude.NFData HttpUrlDestinationProperties where
  rnf HttpUrlDestinationProperties' {..} =
    Prelude.rnf confirmationUrl
