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
-- Module      : Amazonka.OpenSearchServerless.Types.SecurityConfigDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.SecurityConfigDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.SamlConfigOptions
import Amazonka.OpenSearchServerless.Types.SecurityConfigType
import qualified Amazonka.Prelude as Prelude

-- | Details about a security configuration for OpenSearch Serverless.
--
-- /See:/ 'newSecurityConfigDetail' smart constructor.
data SecurityConfigDetail = SecurityConfigDetail'
  { -- | The version of the security configuration.
    configVersion :: Prelude.Maybe Prelude.Text,
    -- | The date the configuration was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The description of the security configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the security configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the configuration was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | SAML options for the security configuration in the form of a key-value
    -- map.
    samlOptions :: Prelude.Maybe SamlConfigOptions,
    -- | The type of security configuration.
    type' :: Prelude.Maybe SecurityConfigType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfigDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configVersion', 'securityConfigDetail_configVersion' - The version of the security configuration.
--
-- 'createdDate', 'securityConfigDetail_createdDate' - The date the configuration was created.
--
-- 'description', 'securityConfigDetail_description' - The description of the security configuration.
--
-- 'id', 'securityConfigDetail_id' - The unique identifier of the security configuration.
--
-- 'lastModifiedDate', 'securityConfigDetail_lastModifiedDate' - The timestamp of when the configuration was last modified.
--
-- 'samlOptions', 'securityConfigDetail_samlOptions' - SAML options for the security configuration in the form of a key-value
-- map.
--
-- 'type'', 'securityConfigDetail_type' - The type of security configuration.
newSecurityConfigDetail ::
  SecurityConfigDetail
newSecurityConfigDetail =
  SecurityConfigDetail'
    { configVersion =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      samlOptions = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The version of the security configuration.
securityConfigDetail_configVersion :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe Prelude.Text)
securityConfigDetail_configVersion = Lens.lens (\SecurityConfigDetail' {configVersion} -> configVersion) (\s@SecurityConfigDetail' {} a -> s {configVersion = a} :: SecurityConfigDetail)

-- | The date the configuration was created.
securityConfigDetail_createdDate :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe Prelude.Integer)
securityConfigDetail_createdDate = Lens.lens (\SecurityConfigDetail' {createdDate} -> createdDate) (\s@SecurityConfigDetail' {} a -> s {createdDate = a} :: SecurityConfigDetail)

-- | The description of the security configuration.
securityConfigDetail_description :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe Prelude.Text)
securityConfigDetail_description = Lens.lens (\SecurityConfigDetail' {description} -> description) (\s@SecurityConfigDetail' {} a -> s {description = a} :: SecurityConfigDetail)

-- | The unique identifier of the security configuration.
securityConfigDetail_id :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe Prelude.Text)
securityConfigDetail_id = Lens.lens (\SecurityConfigDetail' {id} -> id) (\s@SecurityConfigDetail' {} a -> s {id = a} :: SecurityConfigDetail)

-- | The timestamp of when the configuration was last modified.
securityConfigDetail_lastModifiedDate :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe Prelude.Integer)
securityConfigDetail_lastModifiedDate = Lens.lens (\SecurityConfigDetail' {lastModifiedDate} -> lastModifiedDate) (\s@SecurityConfigDetail' {} a -> s {lastModifiedDate = a} :: SecurityConfigDetail)

-- | SAML options for the security configuration in the form of a key-value
-- map.
securityConfigDetail_samlOptions :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe SamlConfigOptions)
securityConfigDetail_samlOptions = Lens.lens (\SecurityConfigDetail' {samlOptions} -> samlOptions) (\s@SecurityConfigDetail' {} a -> s {samlOptions = a} :: SecurityConfigDetail)

-- | The type of security configuration.
securityConfigDetail_type :: Lens.Lens' SecurityConfigDetail (Prelude.Maybe SecurityConfigType)
securityConfigDetail_type = Lens.lens (\SecurityConfigDetail' {type'} -> type') (\s@SecurityConfigDetail' {} a -> s {type' = a} :: SecurityConfigDetail)

instance Data.FromJSON SecurityConfigDetail where
  parseJSON =
    Data.withObject
      "SecurityConfigDetail"
      ( \x ->
          SecurityConfigDetail'
            Prelude.<$> (x Data..:? "configVersion")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "samlOptions")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable SecurityConfigDetail where
  hashWithSalt _salt SecurityConfigDetail' {..} =
    _salt
      `Prelude.hashWithSalt` configVersion
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` samlOptions
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SecurityConfigDetail where
  rnf SecurityConfigDetail' {..} =
    Prelude.rnf configVersion `Prelude.seq`
      Prelude.rnf createdDate `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf lastModifiedDate `Prelude.seq`
              Prelude.rnf samlOptions `Prelude.seq`
                Prelude.rnf type'
