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
-- Module      : Amazonka.Route53AutoNaming.Types.PrivateDnsNamespaceChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PrivateDnsNamespaceChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.PrivateDnsNamespacePropertiesChange

-- | Updated properties for the private DNS namespace.
--
-- /See:/ 'newPrivateDnsNamespaceChange' smart constructor.
data PrivateDnsNamespaceChange = PrivateDnsNamespaceChange'
  { -- | An updated description for the private DNS namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | Properties to be updated in the private DNS namespace.
    properties :: Prelude.Maybe PrivateDnsNamespacePropertiesChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsNamespaceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'privateDnsNamespaceChange_description' - An updated description for the private DNS namespace.
--
-- 'properties', 'privateDnsNamespaceChange_properties' - Properties to be updated in the private DNS namespace.
newPrivateDnsNamespaceChange ::
  PrivateDnsNamespaceChange
newPrivateDnsNamespaceChange =
  PrivateDnsNamespaceChange'
    { description =
        Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | An updated description for the private DNS namespace.
privateDnsNamespaceChange_description :: Lens.Lens' PrivateDnsNamespaceChange (Prelude.Maybe Prelude.Text)
privateDnsNamespaceChange_description = Lens.lens (\PrivateDnsNamespaceChange' {description} -> description) (\s@PrivateDnsNamespaceChange' {} a -> s {description = a} :: PrivateDnsNamespaceChange)

-- | Properties to be updated in the private DNS namespace.
privateDnsNamespaceChange_properties :: Lens.Lens' PrivateDnsNamespaceChange (Prelude.Maybe PrivateDnsNamespacePropertiesChange)
privateDnsNamespaceChange_properties = Lens.lens (\PrivateDnsNamespaceChange' {properties} -> properties) (\s@PrivateDnsNamespaceChange' {} a -> s {properties = a} :: PrivateDnsNamespaceChange)

instance Prelude.Hashable PrivateDnsNamespaceChange where
  hashWithSalt _salt PrivateDnsNamespaceChange' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` properties

instance Prelude.NFData PrivateDnsNamespaceChange where
  rnf PrivateDnsNamespaceChange' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf properties

instance Data.ToJSON PrivateDnsNamespaceChange where
  toJSON PrivateDnsNamespaceChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Properties" Data..=) Prelude.<$> properties
          ]
      )
