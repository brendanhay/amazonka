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
-- Module      : Amazonka.MediaTailor.Types.HttpConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.HttpConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The HTTP configuration for the source location.
--
-- /See:/ 'newHttpConfiguration' smart constructor.
data HttpConfiguration = HttpConfiguration'
  { -- | The base URL for the source location host server. This string must
    -- include the protocol, such as __https:\/\/__.
    baseUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseUrl', 'httpConfiguration_baseUrl' - The base URL for the source location host server. This string must
-- include the protocol, such as __https:\/\/__.
newHttpConfiguration ::
  -- | 'baseUrl'
  Prelude.Text ->
  HttpConfiguration
newHttpConfiguration pBaseUrl_ =
  HttpConfiguration' {baseUrl = pBaseUrl_}

-- | The base URL for the source location host server. This string must
-- include the protocol, such as __https:\/\/__.
httpConfiguration_baseUrl :: Lens.Lens' HttpConfiguration Prelude.Text
httpConfiguration_baseUrl = Lens.lens (\HttpConfiguration' {baseUrl} -> baseUrl) (\s@HttpConfiguration' {} a -> s {baseUrl = a} :: HttpConfiguration)

instance Data.FromJSON HttpConfiguration where
  parseJSON =
    Data.withObject
      "HttpConfiguration"
      ( \x ->
          HttpConfiguration' Prelude.<$> (x Data..: "BaseUrl")
      )

instance Prelude.Hashable HttpConfiguration where
  hashWithSalt _salt HttpConfiguration' {..} =
    _salt `Prelude.hashWithSalt` baseUrl

instance Prelude.NFData HttpConfiguration where
  rnf HttpConfiguration' {..} = Prelude.rnf baseUrl

instance Data.ToJSON HttpConfiguration where
  toJSON HttpConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BaseUrl" Data..= baseUrl)]
      )
