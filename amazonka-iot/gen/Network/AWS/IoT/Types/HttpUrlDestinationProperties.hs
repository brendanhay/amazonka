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
-- Module      : Network.AWS.IoT.Types.HttpUrlDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpUrlDestinationProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | HTTP URL destination properties.
--
-- /See:/ 'newHttpUrlDestinationProperties' smart constructor.
data HttpUrlDestinationProperties = HttpUrlDestinationProperties'
  { -- | The URL used to confirm the HTTP topic rule destination URL.
    confirmationUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    HttpUrlDestinationProperties
  where
  parseJSON =
    Prelude.withObject
      "HttpUrlDestinationProperties"
      ( \x ->
          HttpUrlDestinationProperties'
            Prelude.<$> (x Prelude..:? "confirmationUrl")
      )

instance
  Prelude.Hashable
    HttpUrlDestinationProperties

instance Prelude.NFData HttpUrlDestinationProperties
