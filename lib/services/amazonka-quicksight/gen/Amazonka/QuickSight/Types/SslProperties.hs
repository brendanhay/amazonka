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
-- Module      : Amazonka.QuickSight.Types.SslProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SslProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Secure Socket Layer (SSL) properties that apply when Amazon QuickSight
-- connects to your underlying data source.
--
-- /See:/ 'newSslProperties' smart constructor.
data SslProperties = SslProperties'
  { -- | A Boolean option to control whether SSL should be disabled.
    disableSsl :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SslProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableSsl', 'sslProperties_disableSsl' - A Boolean option to control whether SSL should be disabled.
newSslProperties ::
  SslProperties
newSslProperties =
  SslProperties' {disableSsl = Prelude.Nothing}

-- | A Boolean option to control whether SSL should be disabled.
sslProperties_disableSsl :: Lens.Lens' SslProperties (Prelude.Maybe Prelude.Bool)
sslProperties_disableSsl = Lens.lens (\SslProperties' {disableSsl} -> disableSsl) (\s@SslProperties' {} a -> s {disableSsl = a} :: SslProperties)

instance Data.FromJSON SslProperties where
  parseJSON =
    Data.withObject
      "SslProperties"
      ( \x ->
          SslProperties' Prelude.<$> (x Data..:? "DisableSsl")
      )

instance Prelude.Hashable SslProperties where
  hashWithSalt _salt SslProperties' {..} =
    _salt `Prelude.hashWithSalt` disableSsl

instance Prelude.NFData SslProperties where
  rnf SslProperties' {..} = Prelude.rnf disableSsl

instance Data.ToJSON SslProperties where
  toJSON SslProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [("DisableSsl" Data..=) Prelude.<$> disableSsl]
      )
