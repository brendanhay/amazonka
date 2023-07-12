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
-- Module      : Amazonka.Kendra.Types.SalesforceKnowledgeArticleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceKnowledgeArticleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
import Amazonka.Kendra.Types.SalesforceKnowledgeArticleState
import Amazonka.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for the knowledge article types
-- that Amazon Kendra indexes. Amazon Kendra indexes standard knowledge
-- articles and the standard fields of knowledge articles, or the custom
-- fields of custom knowledge articles, but not both
--
-- /See:/ 'newSalesforceKnowledgeArticleConfiguration' smart constructor.
data SalesforceKnowledgeArticleConfiguration = SalesforceKnowledgeArticleConfiguration'
  { -- | Configuration information for custom Salesforce knowledge articles.
    customKnowledgeArticleTypeConfigurations :: Prelude.Maybe (Prelude.NonEmpty SalesforceCustomKnowledgeArticleTypeConfiguration),
    -- | Configuration information for standard Salesforce knowledge articles.
    standardKnowledgeArticleTypeConfiguration :: Prelude.Maybe SalesforceStandardKnowledgeArticleTypeConfiguration,
    -- | Specifies the document states that should be included when Amazon Kendra
    -- indexes knowledge articles. You must specify at least one state.
    includedStates :: Prelude.NonEmpty SalesforceKnowledgeArticleState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceKnowledgeArticleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customKnowledgeArticleTypeConfigurations', 'salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations' - Configuration information for custom Salesforce knowledge articles.
--
-- 'standardKnowledgeArticleTypeConfiguration', 'salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration' - Configuration information for standard Salesforce knowledge articles.
--
-- 'includedStates', 'salesforceKnowledgeArticleConfiguration_includedStates' - Specifies the document states that should be included when Amazon Kendra
-- indexes knowledge articles. You must specify at least one state.
newSalesforceKnowledgeArticleConfiguration ::
  -- | 'includedStates'
  Prelude.NonEmpty SalesforceKnowledgeArticleState ->
  SalesforceKnowledgeArticleConfiguration
newSalesforceKnowledgeArticleConfiguration
  pIncludedStates_ =
    SalesforceKnowledgeArticleConfiguration'
      { customKnowledgeArticleTypeConfigurations =
          Prelude.Nothing,
        standardKnowledgeArticleTypeConfiguration =
          Prelude.Nothing,
        includedStates =
          Lens.coerced
            Lens.# pIncludedStates_
      }

-- | Configuration information for custom Salesforce knowledge articles.
salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations :: Lens.Lens' SalesforceKnowledgeArticleConfiguration (Prelude.Maybe (Prelude.NonEmpty SalesforceCustomKnowledgeArticleTypeConfiguration))
salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations = Lens.lens (\SalesforceKnowledgeArticleConfiguration' {customKnowledgeArticleTypeConfigurations} -> customKnowledgeArticleTypeConfigurations) (\s@SalesforceKnowledgeArticleConfiguration' {} a -> s {customKnowledgeArticleTypeConfigurations = a} :: SalesforceKnowledgeArticleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for standard Salesforce knowledge articles.
salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration :: Lens.Lens' SalesforceKnowledgeArticleConfiguration (Prelude.Maybe SalesforceStandardKnowledgeArticleTypeConfiguration)
salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration = Lens.lens (\SalesforceKnowledgeArticleConfiguration' {standardKnowledgeArticleTypeConfiguration} -> standardKnowledgeArticleTypeConfiguration) (\s@SalesforceKnowledgeArticleConfiguration' {} a -> s {standardKnowledgeArticleTypeConfiguration = a} :: SalesforceKnowledgeArticleConfiguration)

-- | Specifies the document states that should be included when Amazon Kendra
-- indexes knowledge articles. You must specify at least one state.
salesforceKnowledgeArticleConfiguration_includedStates :: Lens.Lens' SalesforceKnowledgeArticleConfiguration (Prelude.NonEmpty SalesforceKnowledgeArticleState)
salesforceKnowledgeArticleConfiguration_includedStates = Lens.lens (\SalesforceKnowledgeArticleConfiguration' {includedStates} -> includedStates) (\s@SalesforceKnowledgeArticleConfiguration' {} a -> s {includedStates = a} :: SalesforceKnowledgeArticleConfiguration) Prelude.. Lens.coerced

instance
  Data.FromJSON
    SalesforceKnowledgeArticleConfiguration
  where
  parseJSON =
    Data.withObject
      "SalesforceKnowledgeArticleConfiguration"
      ( \x ->
          SalesforceKnowledgeArticleConfiguration'
            Prelude.<$> ( x
                            Data..:? "CustomKnowledgeArticleTypeConfigurations"
                        )
            Prelude.<*> ( x
                            Data..:? "StandardKnowledgeArticleTypeConfiguration"
                        )
            Prelude.<*> (x Data..: "IncludedStates")
      )

instance
  Prelude.Hashable
    SalesforceKnowledgeArticleConfiguration
  where
  hashWithSalt
    _salt
    SalesforceKnowledgeArticleConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customKnowledgeArticleTypeConfigurations
        `Prelude.hashWithSalt` standardKnowledgeArticleTypeConfiguration
        `Prelude.hashWithSalt` includedStates

instance
  Prelude.NFData
    SalesforceKnowledgeArticleConfiguration
  where
  rnf SalesforceKnowledgeArticleConfiguration' {..} =
    Prelude.rnf
      customKnowledgeArticleTypeConfigurations
      `Prelude.seq` Prelude.rnf standardKnowledgeArticleTypeConfiguration
      `Prelude.seq` Prelude.rnf includedStates

instance
  Data.ToJSON
    SalesforceKnowledgeArticleConfiguration
  where
  toJSON SalesforceKnowledgeArticleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomKnowledgeArticleTypeConfigurations" Data..=)
              Prelude.<$> customKnowledgeArticleTypeConfigurations,
            ("StandardKnowledgeArticleTypeConfiguration" Data..=)
              Prelude.<$> standardKnowledgeArticleTypeConfiguration,
            Prelude.Just
              ("IncludedStates" Data..= includedStates)
          ]
      )
