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
-- Module      : Amazonka.NetworkFirewall.Types.TLSInspectionConfigurationMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.TLSInspectionConfigurationMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a TLS inspection configuration, returned by
-- @ListTLSInspectionConfigurations@. You can use the information provided
-- in the metadata to retrieve and manage a TLS configuration.
--
-- /See:/ 'newTLSInspectionConfigurationMetadata' smart constructor.
data TLSInspectionConfigurationMetadata = TLSInspectionConfigurationMetadata'
  { -- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the TLS inspection configuration. You can\'t
    -- change the name of a TLS inspection configuration after you create it.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TLSInspectionConfigurationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'tLSInspectionConfigurationMetadata_arn' - The Amazon Resource Name (ARN) of the TLS inspection configuration.
--
-- 'name', 'tLSInspectionConfigurationMetadata_name' - The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
newTLSInspectionConfigurationMetadata ::
  TLSInspectionConfigurationMetadata
newTLSInspectionConfigurationMetadata =
  TLSInspectionConfigurationMetadata'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the TLS inspection configuration.
tLSInspectionConfigurationMetadata_arn :: Lens.Lens' TLSInspectionConfigurationMetadata (Prelude.Maybe Prelude.Text)
tLSInspectionConfigurationMetadata_arn = Lens.lens (\TLSInspectionConfigurationMetadata' {arn} -> arn) (\s@TLSInspectionConfigurationMetadata' {} a -> s {arn = a} :: TLSInspectionConfigurationMetadata)

-- | The descriptive name of the TLS inspection configuration. You can\'t
-- change the name of a TLS inspection configuration after you create it.
tLSInspectionConfigurationMetadata_name :: Lens.Lens' TLSInspectionConfigurationMetadata (Prelude.Maybe Prelude.Text)
tLSInspectionConfigurationMetadata_name = Lens.lens (\TLSInspectionConfigurationMetadata' {name} -> name) (\s@TLSInspectionConfigurationMetadata' {} a -> s {name = a} :: TLSInspectionConfigurationMetadata)

instance
  Data.FromJSON
    TLSInspectionConfigurationMetadata
  where
  parseJSON =
    Data.withObject
      "TLSInspectionConfigurationMetadata"
      ( \x ->
          TLSInspectionConfigurationMetadata'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    TLSInspectionConfigurationMetadata
  where
  hashWithSalt
    _salt
    TLSInspectionConfigurationMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    TLSInspectionConfigurationMetadata
  where
  rnf TLSInspectionConfigurationMetadata' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf name
