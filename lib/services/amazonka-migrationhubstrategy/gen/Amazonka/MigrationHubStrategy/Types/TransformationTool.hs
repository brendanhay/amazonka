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
-- Module      : Amazonka.MigrationHubStrategy.Types.TransformationTool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.TransformationTool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.TransformationToolName
import qualified Amazonka.Prelude as Prelude

-- | Information of the transformation tool that can be used to migrate and
-- modernize the application.
--
-- /See:/ 'newTransformationTool' smart constructor.
data TransformationTool = TransformationTool'
  { -- | Name of the tool.
    name :: Prelude.Maybe TransformationToolName,
    -- | URL for installing the tool.
    tranformationToolInstallationLink :: Prelude.Maybe Prelude.Text,
    -- | Description of the tool.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformationTool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'transformationTool_name' - Name of the tool.
--
-- 'tranformationToolInstallationLink', 'transformationTool_tranformationToolInstallationLink' - URL for installing the tool.
--
-- 'description', 'transformationTool_description' - Description of the tool.
newTransformationTool ::
  TransformationTool
newTransformationTool =
  TransformationTool'
    { name = Prelude.Nothing,
      tranformationToolInstallationLink = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Name of the tool.
transformationTool_name :: Lens.Lens' TransformationTool (Prelude.Maybe TransformationToolName)
transformationTool_name = Lens.lens (\TransformationTool' {name} -> name) (\s@TransformationTool' {} a -> s {name = a} :: TransformationTool)

-- | URL for installing the tool.
transformationTool_tranformationToolInstallationLink :: Lens.Lens' TransformationTool (Prelude.Maybe Prelude.Text)
transformationTool_tranformationToolInstallationLink = Lens.lens (\TransformationTool' {tranformationToolInstallationLink} -> tranformationToolInstallationLink) (\s@TransformationTool' {} a -> s {tranformationToolInstallationLink = a} :: TransformationTool)

-- | Description of the tool.
transformationTool_description :: Lens.Lens' TransformationTool (Prelude.Maybe Prelude.Text)
transformationTool_description = Lens.lens (\TransformationTool' {description} -> description) (\s@TransformationTool' {} a -> s {description = a} :: TransformationTool)

instance Data.FromJSON TransformationTool where
  parseJSON =
    Data.withObject
      "TransformationTool"
      ( \x ->
          TransformationTool'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tranformationToolInstallationLink")
            Prelude.<*> (x Data..:? "description")
      )

instance Prelude.Hashable TransformationTool where
  hashWithSalt _salt TransformationTool' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tranformationToolInstallationLink
      `Prelude.hashWithSalt` description

instance Prelude.NFData TransformationTool where
  rnf TransformationTool' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf tranformationToolInstallationLink
      `Prelude.seq` Prelude.rnf description
