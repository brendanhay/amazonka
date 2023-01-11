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
-- Module      : Amazonka.OpenSearchServerless.Types.SecurityConfigSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SecurityConfigSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.SecurityConfigType
import qualified Amazonka.Prelude as Prelude

-- | A summary of a security configuration for OpenSearch Serverless.
--
-- /See:/ 'newSecurityConfigSummary' smart constructor.
data SecurityConfigSummary = SecurityConfigSummary'
  { -- | The version of the security configuration.
    configVersion :: Prelude.Maybe Prelude.Text,
    -- | The Epoch time when the security configuration was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the security configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the security configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the configuration was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The type of security configuration.
    type' :: Prelude.Maybe SecurityConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfigSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configVersion', 'securityConfigSummary_configVersion' - The version of the security configuration.
--
-- 'createdDate', 'securityConfigSummary_createdDate' - The Epoch time when the security configuration was created.
--
-- 'description', 'securityConfigSummary_description' - The description of the security configuration.
--
-- 'id', 'securityConfigSummary_id' - The unique identifier of the security configuration.
--
-- 'lastModifiedDate', 'securityConfigSummary_lastModifiedDate' - The timestamp of when the configuration was last modified.
--
-- 'type'', 'securityConfigSummary_type' - The type of security configuration.
newSecurityConfigSummary ::
  SecurityConfigSummary
newSecurityConfigSummary =
  SecurityConfigSummary'
    { configVersion =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The version of the security configuration.
securityConfigSummary_configVersion :: Lens.Lens' SecurityConfigSummary (Prelude.Maybe Prelude.Text)
securityConfigSummary_configVersion = Lens.lens (\SecurityConfigSummary' {configVersion} -> configVersion) (\s@SecurityConfigSummary' {} a -> s {configVersion = a} :: SecurityConfigSummary)

-- | The Epoch time when the security configuration was created.
securityConfigSummary_createdDate :: Lens.Lens' SecurityConfigSummary (Prelude.Maybe Prelude.Integer)
securityConfigSummary_createdDate = Lens.lens (\SecurityConfigSummary' {createdDate} -> createdDate) (\s@SecurityConfigSummary' {} a -> s {createdDate = a} :: SecurityConfigSummary)

-- | The description of the security configuration.
securityConfigSummary_description :: Lens.Lens' SecurityConfigSummary (Prelude.Maybe Prelude.Text)
securityConfigSummary_description = Lens.lens (\SecurityConfigSummary' {description} -> description) (\s@SecurityConfigSummary' {} a -> s {description = a} :: SecurityConfigSummary)

-- | The unique identifier of the security configuration.
securityConfigSummary_id :: Lens.Lens' SecurityConfigSummary (Prelude.Maybe Prelude.Text)
securityConfigSummary_id = Lens.lens (\SecurityConfigSummary' {id} -> id) (\s@SecurityConfigSummary' {} a -> s {id = a} :: SecurityConfigSummary)

-- | The timestamp of when the configuration was last modified.
securityConfigSummary_lastModifiedDate :: Lens.Lens' SecurityConfigSummary (Prelude.Maybe Prelude.Integer)
securityConfigSummary_lastModifiedDate = Lens.lens (\SecurityConfigSummary' {lastModifiedDate} -> lastModifiedDate) (\s@SecurityConfigSummary' {} a -> s {lastModifiedDate = a} :: SecurityConfigSummary)

-- | The type of security configuration.
securityConfigSummary_type :: Lens.Lens' SecurityConfigSummary (Prelude.Maybe SecurityConfigType)
securityConfigSummary_type = Lens.lens (\SecurityConfigSummary' {type'} -> type') (\s@SecurityConfigSummary' {} a -> s {type' = a} :: SecurityConfigSummary)

instance Data.FromJSON SecurityConfigSummary where
  parseJSON =
    Data.withObject
      "SecurityConfigSummary"
      ( \x ->
          SecurityConfigSummary'
            Prelude.<$> (x Data..:? "configVersion")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable SecurityConfigSummary where
  hashWithSalt _salt SecurityConfigSummary' {..} =
    _salt `Prelude.hashWithSalt` configVersion
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SecurityConfigSummary where
  rnf SecurityConfigSummary' {..} =
    Prelude.rnf configVersion
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf type'
