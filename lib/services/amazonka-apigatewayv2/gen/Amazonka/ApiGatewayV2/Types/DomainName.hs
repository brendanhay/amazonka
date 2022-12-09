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
-- Module      : Amazonka.ApiGatewayV2.Types.DomainName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.DomainName where

import Amazonka.ApiGatewayV2.Types.DomainNameConfiguration
import Amazonka.ApiGatewayV2.Types.MutualTlsAuthentication
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a domain name.
--
-- /See:/ 'newDomainName' smart constructor.
data DomainName = DomainName'
  { -- | The API mapping selection expression.
    apiMappingSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The domain name configurations.
    domainNameConfigurations :: Prelude.Maybe [DomainNameConfiguration],
    -- | The mutual TLS authentication configuration for a custom domain name.
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthentication,
    -- | The collection of tags associated with a domain name.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the DomainName resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingSelectionExpression', 'domainName_apiMappingSelectionExpression' - The API mapping selection expression.
--
-- 'domainNameConfigurations', 'domainName_domainNameConfigurations' - The domain name configurations.
--
-- 'mutualTlsAuthentication', 'domainName_mutualTlsAuthentication' - The mutual TLS authentication configuration for a custom domain name.
--
-- 'tags', 'domainName_tags' - The collection of tags associated with a domain name.
--
-- 'domainName', 'domainName_domainName' - The name of the DomainName resource.
newDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  DomainName
newDomainName pDomainName_ =
  DomainName'
    { apiMappingSelectionExpression =
        Prelude.Nothing,
      domainNameConfigurations = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      tags = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The API mapping selection expression.
domainName_apiMappingSelectionExpression :: Lens.Lens' DomainName (Prelude.Maybe Prelude.Text)
domainName_apiMappingSelectionExpression = Lens.lens (\DomainName' {apiMappingSelectionExpression} -> apiMappingSelectionExpression) (\s@DomainName' {} a -> s {apiMappingSelectionExpression = a} :: DomainName)

-- | The domain name configurations.
domainName_domainNameConfigurations :: Lens.Lens' DomainName (Prelude.Maybe [DomainNameConfiguration])
domainName_domainNameConfigurations = Lens.lens (\DomainName' {domainNameConfigurations} -> domainNameConfigurations) (\s@DomainName' {} a -> s {domainNameConfigurations = a} :: DomainName) Prelude.. Lens.mapping Lens.coerced

-- | The mutual TLS authentication configuration for a custom domain name.
domainName_mutualTlsAuthentication :: Lens.Lens' DomainName (Prelude.Maybe MutualTlsAuthentication)
domainName_mutualTlsAuthentication = Lens.lens (\DomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@DomainName' {} a -> s {mutualTlsAuthentication = a} :: DomainName)

-- | The collection of tags associated with a domain name.
domainName_tags :: Lens.Lens' DomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
domainName_tags = Lens.lens (\DomainName' {tags} -> tags) (\s@DomainName' {} a -> s {tags = a} :: DomainName) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DomainName resource.
domainName_domainName :: Lens.Lens' DomainName Prelude.Text
domainName_domainName = Lens.lens (\DomainName' {domainName} -> domainName) (\s@DomainName' {} a -> s {domainName = a} :: DomainName)

instance Data.FromJSON DomainName where
  parseJSON =
    Data.withObject
      "DomainName"
      ( \x ->
          DomainName'
            Prelude.<$> (x Data..:? "apiMappingSelectionExpression")
            Prelude.<*> ( x Data..:? "domainNameConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "mutualTlsAuthentication")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "domainName")
      )

instance Prelude.Hashable DomainName where
  hashWithSalt _salt DomainName' {..} =
    _salt
      `Prelude.hashWithSalt` apiMappingSelectionExpression
      `Prelude.hashWithSalt` domainNameConfigurations
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DomainName where
  rnf DomainName' {..} =
    Prelude.rnf apiMappingSelectionExpression
      `Prelude.seq` Prelude.rnf domainNameConfigurations
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName
