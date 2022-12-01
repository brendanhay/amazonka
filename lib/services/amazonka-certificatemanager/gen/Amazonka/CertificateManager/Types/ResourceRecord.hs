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
-- Module      : Amazonka.CertificateManager.Types.ResourceRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.ResourceRecord where

import Amazonka.CertificateManager.Types.RecordType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a DNS record value that you can use to validate ownership or
-- control of a domain. This is used by the DescribeCertificate action.
--
-- /See:/ 'newResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The name of the DNS record to create in your domain. This is supplied by
    -- ACM.
    name :: Prelude.Text,
    -- | The type of DNS record. Currently this can be @CNAME@.
    type' :: RecordType,
    -- | The value of the CNAME record to add to your DNS database. This is
    -- supplied by ACM.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceRecord_name' - The name of the DNS record to create in your domain. This is supplied by
-- ACM.
--
-- 'type'', 'resourceRecord_type' - The type of DNS record. Currently this can be @CNAME@.
--
-- 'value', 'resourceRecord_value' - The value of the CNAME record to add to your DNS database. This is
-- supplied by ACM.
newResourceRecord ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  RecordType ->
  -- | 'value'
  Prelude.Text ->
  ResourceRecord
newResourceRecord pName_ pType_ pValue_ =
  ResourceRecord'
    { name = pName_,
      type' = pType_,
      value = pValue_
    }

-- | The name of the DNS record to create in your domain. This is supplied by
-- ACM.
resourceRecord_name :: Lens.Lens' ResourceRecord Prelude.Text
resourceRecord_name = Lens.lens (\ResourceRecord' {name} -> name) (\s@ResourceRecord' {} a -> s {name = a} :: ResourceRecord)

-- | The type of DNS record. Currently this can be @CNAME@.
resourceRecord_type :: Lens.Lens' ResourceRecord RecordType
resourceRecord_type = Lens.lens (\ResourceRecord' {type'} -> type') (\s@ResourceRecord' {} a -> s {type' = a} :: ResourceRecord)

-- | The value of the CNAME record to add to your DNS database. This is
-- supplied by ACM.
resourceRecord_value :: Lens.Lens' ResourceRecord Prelude.Text
resourceRecord_value = Lens.lens (\ResourceRecord' {value} -> value) (\s@ResourceRecord' {} a -> s {value = a} :: ResourceRecord)

instance Core.FromJSON ResourceRecord where
  parseJSON =
    Core.withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "Type")
            Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable ResourceRecord where
  hashWithSalt _salt ResourceRecord' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData ResourceRecord where
  rnf ResourceRecord' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
