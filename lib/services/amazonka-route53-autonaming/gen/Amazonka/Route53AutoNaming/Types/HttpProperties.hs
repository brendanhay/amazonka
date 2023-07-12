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
-- Module      : Amazonka.Route53AutoNaming.Types.HttpProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.HttpProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains the name of an HTTP namespace.
--
-- /See:/ 'newHttpProperties' smart constructor.
data HttpProperties = HttpProperties'
  { -- | The name of an HTTP namespace.
    httpName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpName', 'httpProperties_httpName' - The name of an HTTP namespace.
newHttpProperties ::
  HttpProperties
newHttpProperties =
  HttpProperties' {httpName = Prelude.Nothing}

-- | The name of an HTTP namespace.
httpProperties_httpName :: Lens.Lens' HttpProperties (Prelude.Maybe Prelude.Text)
httpProperties_httpName = Lens.lens (\HttpProperties' {httpName} -> httpName) (\s@HttpProperties' {} a -> s {httpName = a} :: HttpProperties)

instance Data.FromJSON HttpProperties where
  parseJSON =
    Data.withObject
      "HttpProperties"
      ( \x ->
          HttpProperties' Prelude.<$> (x Data..:? "HttpName")
      )

instance Prelude.Hashable HttpProperties where
  hashWithSalt _salt HttpProperties' {..} =
    _salt `Prelude.hashWithSalt` httpName

instance Prelude.NFData HttpProperties where
  rnf HttpProperties' {..} = Prelude.rnf httpName
