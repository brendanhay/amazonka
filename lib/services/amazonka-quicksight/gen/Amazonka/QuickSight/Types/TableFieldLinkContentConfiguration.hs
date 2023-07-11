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
-- Module      : Amazonka.QuickSight.Types.TableFieldLinkContentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldLinkContentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldCustomIconContent
import Amazonka.QuickSight.Types.TableFieldCustomTextContent

-- | The URL content (text, icon) for the table link configuration.
--
-- /See:/ 'newTableFieldLinkContentConfiguration' smart constructor.
data TableFieldLinkContentConfiguration = TableFieldLinkContentConfiguration'
  { -- | The custom icon content for the table link content configuration.
    customIconContent :: Prelude.Maybe TableFieldCustomIconContent,
    -- | The custom text content (value, font configuration) for the table link
    -- content configuration.
    customTextContent :: Prelude.Maybe TableFieldCustomTextContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldLinkContentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customIconContent', 'tableFieldLinkContentConfiguration_customIconContent' - The custom icon content for the table link content configuration.
--
-- 'customTextContent', 'tableFieldLinkContentConfiguration_customTextContent' - The custom text content (value, font configuration) for the table link
-- content configuration.
newTableFieldLinkContentConfiguration ::
  TableFieldLinkContentConfiguration
newTableFieldLinkContentConfiguration =
  TableFieldLinkContentConfiguration'
    { customIconContent =
        Prelude.Nothing,
      customTextContent = Prelude.Nothing
    }

-- | The custom icon content for the table link content configuration.
tableFieldLinkContentConfiguration_customIconContent :: Lens.Lens' TableFieldLinkContentConfiguration (Prelude.Maybe TableFieldCustomIconContent)
tableFieldLinkContentConfiguration_customIconContent = Lens.lens (\TableFieldLinkContentConfiguration' {customIconContent} -> customIconContent) (\s@TableFieldLinkContentConfiguration' {} a -> s {customIconContent = a} :: TableFieldLinkContentConfiguration)

-- | The custom text content (value, font configuration) for the table link
-- content configuration.
tableFieldLinkContentConfiguration_customTextContent :: Lens.Lens' TableFieldLinkContentConfiguration (Prelude.Maybe TableFieldCustomTextContent)
tableFieldLinkContentConfiguration_customTextContent = Lens.lens (\TableFieldLinkContentConfiguration' {customTextContent} -> customTextContent) (\s@TableFieldLinkContentConfiguration' {} a -> s {customTextContent = a} :: TableFieldLinkContentConfiguration)

instance
  Data.FromJSON
    TableFieldLinkContentConfiguration
  where
  parseJSON =
    Data.withObject
      "TableFieldLinkContentConfiguration"
      ( \x ->
          TableFieldLinkContentConfiguration'
            Prelude.<$> (x Data..:? "CustomIconContent")
            Prelude.<*> (x Data..:? "CustomTextContent")
      )

instance
  Prelude.Hashable
    TableFieldLinkContentConfiguration
  where
  hashWithSalt
    _salt
    TableFieldLinkContentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customIconContent
        `Prelude.hashWithSalt` customTextContent

instance
  Prelude.NFData
    TableFieldLinkContentConfiguration
  where
  rnf TableFieldLinkContentConfiguration' {..} =
    Prelude.rnf customIconContent
      `Prelude.seq` Prelude.rnf customTextContent

instance
  Data.ToJSON
    TableFieldLinkContentConfiguration
  where
  toJSON TableFieldLinkContentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomIconContent" Data..=)
              Prelude.<$> customIconContent,
            ("CustomTextContent" Data..=)
              Prelude.<$> customTextContent
          ]
      )
