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
-- Module      : Amazonka.Route53AutoNaming.Types.HttpNamespaceChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.HttpNamespaceChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updated properties for the HTTP namespace.
--
-- /See:/ 'newHttpNamespaceChange' smart constructor.
data HttpNamespaceChange = HttpNamespaceChange'
  { -- | An updated description for the HTTP namespace.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpNamespaceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'httpNamespaceChange_description' - An updated description for the HTTP namespace.
newHttpNamespaceChange ::
  -- | 'description'
  Prelude.Text ->
  HttpNamespaceChange
newHttpNamespaceChange pDescription_ =
  HttpNamespaceChange' {description = pDescription_}

-- | An updated description for the HTTP namespace.
httpNamespaceChange_description :: Lens.Lens' HttpNamespaceChange Prelude.Text
httpNamespaceChange_description = Lens.lens (\HttpNamespaceChange' {description} -> description) (\s@HttpNamespaceChange' {} a -> s {description = a} :: HttpNamespaceChange)

instance Prelude.Hashable HttpNamespaceChange where
  hashWithSalt _salt HttpNamespaceChange' {..} =
    _salt `Prelude.hashWithSalt` description

instance Prelude.NFData HttpNamespaceChange where
  rnf HttpNamespaceChange' {..} =
    Prelude.rnf description

instance Data.ToJSON HttpNamespaceChange where
  toJSON HttpNamespaceChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Description" Data..= description)]
      )
