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
-- Module      : Amazonka.Kendra.Types.ContentSourceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ContentSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for your content sources, such as
-- data sources, FAQs, and content indexed directly via
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchPutDocument.html BatchPutDocument>.
--
-- /See:/ 'newContentSourceConfiguration' smart constructor.
data ContentSourceConfiguration = ContentSourceConfiguration'
  { -- | The identifier of the data sources you want to use for your Amazon
    -- Kendra experience.
    dataSourceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | @TRUE@ to use documents you indexed directly using the
    -- @BatchPutDocument@ API.
    directPutContent :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the FAQs that you want to use for your Amazon Kendra
    -- experience.
    faqIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceIds', 'contentSourceConfiguration_dataSourceIds' - The identifier of the data sources you want to use for your Amazon
-- Kendra experience.
--
-- 'directPutContent', 'contentSourceConfiguration_directPutContent' - @TRUE@ to use documents you indexed directly using the
-- @BatchPutDocument@ API.
--
-- 'faqIds', 'contentSourceConfiguration_faqIds' - The identifier of the FAQs that you want to use for your Amazon Kendra
-- experience.
newContentSourceConfiguration ::
  ContentSourceConfiguration
newContentSourceConfiguration =
  ContentSourceConfiguration'
    { dataSourceIds =
        Prelude.Nothing,
      directPutContent = Prelude.Nothing,
      faqIds = Prelude.Nothing
    }

-- | The identifier of the data sources you want to use for your Amazon
-- Kendra experience.
contentSourceConfiguration_dataSourceIds :: Lens.Lens' ContentSourceConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
contentSourceConfiguration_dataSourceIds = Lens.lens (\ContentSourceConfiguration' {dataSourceIds} -> dataSourceIds) (\s@ContentSourceConfiguration' {} a -> s {dataSourceIds = a} :: ContentSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to use documents you indexed directly using the
-- @BatchPutDocument@ API.
contentSourceConfiguration_directPutContent :: Lens.Lens' ContentSourceConfiguration (Prelude.Maybe Prelude.Bool)
contentSourceConfiguration_directPutContent = Lens.lens (\ContentSourceConfiguration' {directPutContent} -> directPutContent) (\s@ContentSourceConfiguration' {} a -> s {directPutContent = a} :: ContentSourceConfiguration)

-- | The identifier of the FAQs that you want to use for your Amazon Kendra
-- experience.
contentSourceConfiguration_faqIds :: Lens.Lens' ContentSourceConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
contentSourceConfiguration_faqIds = Lens.lens (\ContentSourceConfiguration' {faqIds} -> faqIds) (\s@ContentSourceConfiguration' {} a -> s {faqIds = a} :: ContentSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContentSourceConfiguration where
  parseJSON =
    Data.withObject
      "ContentSourceConfiguration"
      ( \x ->
          ContentSourceConfiguration'
            Prelude.<$> (x Data..:? "DataSourceIds")
            Prelude.<*> (x Data..:? "DirectPutContent")
            Prelude.<*> (x Data..:? "FaqIds")
      )

instance Prelude.Hashable ContentSourceConfiguration where
  hashWithSalt _salt ContentSourceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dataSourceIds
      `Prelude.hashWithSalt` directPutContent
      `Prelude.hashWithSalt` faqIds

instance Prelude.NFData ContentSourceConfiguration where
  rnf ContentSourceConfiguration' {..} =
    Prelude.rnf dataSourceIds
      `Prelude.seq` Prelude.rnf directPutContent
      `Prelude.seq` Prelude.rnf faqIds

instance Data.ToJSON ContentSourceConfiguration where
  toJSON ContentSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceIds" Data..=) Prelude.<$> dataSourceIds,
            ("DirectPutContent" Data..=)
              Prelude.<$> directPutContent,
            ("FaqIds" Data..=) Prelude.<$> faqIds
          ]
      )
