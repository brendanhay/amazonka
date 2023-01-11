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
-- Module      : Amazonka.Route53AutoNaming.Types.PublicDnsNamespaceChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PublicDnsNamespaceChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.PublicDnsNamespacePropertiesChange

-- | Updated properties for the public DNS namespace.
--
-- /See:/ 'newPublicDnsNamespaceChange' smart constructor.
data PublicDnsNamespaceChange = PublicDnsNamespaceChange'
  { -- | An updated description for the public DNS namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | Properties to be updated in the public DNS namespace.
    properties :: Prelude.Maybe PublicDnsNamespacePropertiesChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicDnsNamespaceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'publicDnsNamespaceChange_description' - An updated description for the public DNS namespace.
--
-- 'properties', 'publicDnsNamespaceChange_properties' - Properties to be updated in the public DNS namespace.
newPublicDnsNamespaceChange ::
  PublicDnsNamespaceChange
newPublicDnsNamespaceChange =
  PublicDnsNamespaceChange'
    { description =
        Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | An updated description for the public DNS namespace.
publicDnsNamespaceChange_description :: Lens.Lens' PublicDnsNamespaceChange (Prelude.Maybe Prelude.Text)
publicDnsNamespaceChange_description = Lens.lens (\PublicDnsNamespaceChange' {description} -> description) (\s@PublicDnsNamespaceChange' {} a -> s {description = a} :: PublicDnsNamespaceChange)

-- | Properties to be updated in the public DNS namespace.
publicDnsNamespaceChange_properties :: Lens.Lens' PublicDnsNamespaceChange (Prelude.Maybe PublicDnsNamespacePropertiesChange)
publicDnsNamespaceChange_properties = Lens.lens (\PublicDnsNamespaceChange' {properties} -> properties) (\s@PublicDnsNamespaceChange' {} a -> s {properties = a} :: PublicDnsNamespaceChange)

instance Prelude.Hashable PublicDnsNamespaceChange where
  hashWithSalt _salt PublicDnsNamespaceChange' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` properties

instance Prelude.NFData PublicDnsNamespaceChange where
  rnf PublicDnsNamespaceChange' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf properties

instance Data.ToJSON PublicDnsNamespaceChange where
  toJSON PublicDnsNamespaceChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Properties" Data..=) Prelude.<$> properties
          ]
      )
