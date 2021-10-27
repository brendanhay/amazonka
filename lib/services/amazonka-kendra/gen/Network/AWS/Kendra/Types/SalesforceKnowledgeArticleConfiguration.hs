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
-- Module      : Network.AWS.Kendra.Types.SalesforceKnowledgeArticleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.SalesforceKnowledgeArticleConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
import Network.AWS.Kendra.Types.SalesforceKnowledgeArticleState
import Network.AWS.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies configuration information for the knowledge article types that
-- Amazon Kendra indexes. Amazon Kendra indexes standard knowledge articles
-- and the standard fields of knowledge articles, or the custom fields of
-- custom knowledge articles, but not both
--
-- /See:/ 'newSalesforceKnowledgeArticleConfiguration' smart constructor.
data SalesforceKnowledgeArticleConfiguration = SalesforceKnowledgeArticleConfiguration'
  { -- | Provides configuration information for custom Salesforce knowledge
    -- articles.
    customKnowledgeArticleTypeConfigurations :: Prelude.Maybe (Prelude.NonEmpty SalesforceCustomKnowledgeArticleTypeConfiguration),
    -- | Provides configuration information for standard Salesforce knowledge
    -- articles.
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
-- 'customKnowledgeArticleTypeConfigurations', 'salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations' - Provides configuration information for custom Salesforce knowledge
-- articles.
--
-- 'standardKnowledgeArticleTypeConfiguration', 'salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration' - Provides configuration information for standard Salesforce knowledge
-- articles.
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

-- | Provides configuration information for custom Salesforce knowledge
-- articles.
salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations :: Lens.Lens' SalesforceKnowledgeArticleConfiguration (Prelude.Maybe (Prelude.NonEmpty SalesforceCustomKnowledgeArticleTypeConfiguration))
salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations = Lens.lens (\SalesforceKnowledgeArticleConfiguration' {customKnowledgeArticleTypeConfigurations} -> customKnowledgeArticleTypeConfigurations) (\s@SalesforceKnowledgeArticleConfiguration' {} a -> s {customKnowledgeArticleTypeConfigurations = a} :: SalesforceKnowledgeArticleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Provides configuration information for standard Salesforce knowledge
-- articles.
salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration :: Lens.Lens' SalesforceKnowledgeArticleConfiguration (Prelude.Maybe SalesforceStandardKnowledgeArticleTypeConfiguration)
salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration = Lens.lens (\SalesforceKnowledgeArticleConfiguration' {standardKnowledgeArticleTypeConfiguration} -> standardKnowledgeArticleTypeConfiguration) (\s@SalesforceKnowledgeArticleConfiguration' {} a -> s {standardKnowledgeArticleTypeConfiguration = a} :: SalesforceKnowledgeArticleConfiguration)

-- | Specifies the document states that should be included when Amazon Kendra
-- indexes knowledge articles. You must specify at least one state.
salesforceKnowledgeArticleConfiguration_includedStates :: Lens.Lens' SalesforceKnowledgeArticleConfiguration (Prelude.NonEmpty SalesforceKnowledgeArticleState)
salesforceKnowledgeArticleConfiguration_includedStates = Lens.lens (\SalesforceKnowledgeArticleConfiguration' {includedStates} -> includedStates) (\s@SalesforceKnowledgeArticleConfiguration' {} a -> s {includedStates = a} :: SalesforceKnowledgeArticleConfiguration) Prelude.. Lens.coerced

instance
  Core.FromJSON
    SalesforceKnowledgeArticleConfiguration
  where
  parseJSON =
    Core.withObject
      "SalesforceKnowledgeArticleConfiguration"
      ( \x ->
          SalesforceKnowledgeArticleConfiguration'
            Prelude.<$> ( x
                            Core..:? "CustomKnowledgeArticleTypeConfigurations"
                        )
            Prelude.<*> ( x
                            Core..:? "StandardKnowledgeArticleTypeConfiguration"
                        )
            Prelude.<*> (x Core..: "IncludedStates")
      )

instance
  Prelude.Hashable
    SalesforceKnowledgeArticleConfiguration

instance
  Prelude.NFData
    SalesforceKnowledgeArticleConfiguration

instance
  Core.ToJSON
    SalesforceKnowledgeArticleConfiguration
  where
  toJSON SalesforceKnowledgeArticleConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomKnowledgeArticleTypeConfigurations" Core..=)
              Prelude.<$> customKnowledgeArticleTypeConfigurations,
            ("StandardKnowledgeArticleTypeConfiguration" Core..=)
              Prelude.<$> standardKnowledgeArticleTypeConfiguration,
            Prelude.Just
              ("IncludedStates" Core..= includedStates)
          ]
      )
