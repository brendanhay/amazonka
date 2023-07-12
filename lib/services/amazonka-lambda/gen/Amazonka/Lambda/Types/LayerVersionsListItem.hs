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
-- Module      : Amazonka.Lambda.Types.LayerVersionsListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.LayerVersionsListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.Architecture
import Amazonka.Lambda.Types.Runtime
import qualified Amazonka.Prelude as Prelude

-- | Details about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
--
-- /See:/ 'newLayerVersionsListItem' smart constructor.
data LayerVersionsListItem = LayerVersionsListItem'
  { -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
    compatibleArchitectures :: Prelude.Maybe [Architecture],
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Prelude.Maybe [Runtime],
    -- | The date that the version was created, in ISO 8601 format. For example,
    -- @2018-11-27T15:10:45.123+0000@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the layer version.
    layerVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s open-source license.
    licenseInfo :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayerVersionsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleArchitectures', 'layerVersionsListItem_compatibleArchitectures' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
--
-- 'compatibleRuntimes', 'layerVersionsListItem_compatibleRuntimes' - The layer\'s compatible runtimes.
--
-- 'createdDate', 'layerVersionsListItem_createdDate' - The date that the version was created, in ISO 8601 format. For example,
-- @2018-11-27T15:10:45.123+0000@.
--
-- 'description', 'layerVersionsListItem_description' - The description of the version.
--
-- 'layerVersionArn', 'layerVersionsListItem_layerVersionArn' - The ARN of the layer version.
--
-- 'licenseInfo', 'layerVersionsListItem_licenseInfo' - The layer\'s open-source license.
--
-- 'version', 'layerVersionsListItem_version' - The version number.
newLayerVersionsListItem ::
  LayerVersionsListItem
newLayerVersionsListItem =
  LayerVersionsListItem'
    { compatibleArchitectures =
        Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      layerVersionArn = Prelude.Nothing,
      licenseInfo = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
layerVersionsListItem_compatibleArchitectures :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe [Architecture])
layerVersionsListItem_compatibleArchitectures = Lens.lens (\LayerVersionsListItem' {compatibleArchitectures} -> compatibleArchitectures) (\s@LayerVersionsListItem' {} a -> s {compatibleArchitectures = a} :: LayerVersionsListItem) Prelude.. Lens.mapping Lens.coerced

-- | The layer\'s compatible runtimes.
layerVersionsListItem_compatibleRuntimes :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe [Runtime])
layerVersionsListItem_compatibleRuntimes = Lens.lens (\LayerVersionsListItem' {compatibleRuntimes} -> compatibleRuntimes) (\s@LayerVersionsListItem' {} a -> s {compatibleRuntimes = a} :: LayerVersionsListItem) Prelude.. Lens.mapping Lens.coerced

-- | The date that the version was created, in ISO 8601 format. For example,
-- @2018-11-27T15:10:45.123+0000@.
layerVersionsListItem_createdDate :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_createdDate = Lens.lens (\LayerVersionsListItem' {createdDate} -> createdDate) (\s@LayerVersionsListItem' {} a -> s {createdDate = a} :: LayerVersionsListItem)

-- | The description of the version.
layerVersionsListItem_description :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_description = Lens.lens (\LayerVersionsListItem' {description} -> description) (\s@LayerVersionsListItem' {} a -> s {description = a} :: LayerVersionsListItem)

-- | The ARN of the layer version.
layerVersionsListItem_layerVersionArn :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_layerVersionArn = Lens.lens (\LayerVersionsListItem' {layerVersionArn} -> layerVersionArn) (\s@LayerVersionsListItem' {} a -> s {layerVersionArn = a} :: LayerVersionsListItem)

-- | The layer\'s open-source license.
layerVersionsListItem_licenseInfo :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_licenseInfo = Lens.lens (\LayerVersionsListItem' {licenseInfo} -> licenseInfo) (\s@LayerVersionsListItem' {} a -> s {licenseInfo = a} :: LayerVersionsListItem)

-- | The version number.
layerVersionsListItem_version :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Integer)
layerVersionsListItem_version = Lens.lens (\LayerVersionsListItem' {version} -> version) (\s@LayerVersionsListItem' {} a -> s {version = a} :: LayerVersionsListItem)

instance Data.FromJSON LayerVersionsListItem where
  parseJSON =
    Data.withObject
      "LayerVersionsListItem"
      ( \x ->
          LayerVersionsListItem'
            Prelude.<$> ( x
                            Data..:? "CompatibleArchitectures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "CompatibleRuntimes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LayerVersionArn")
            Prelude.<*> (x Data..:? "LicenseInfo")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable LayerVersionsListItem where
  hashWithSalt _salt LayerVersionsListItem' {..} =
    _salt
      `Prelude.hashWithSalt` compatibleArchitectures
      `Prelude.hashWithSalt` compatibleRuntimes
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` layerVersionArn
      `Prelude.hashWithSalt` licenseInfo
      `Prelude.hashWithSalt` version

instance Prelude.NFData LayerVersionsListItem where
  rnf LayerVersionsListItem' {..} =
    Prelude.rnf compatibleArchitectures
      `Prelude.seq` Prelude.rnf compatibleRuntimes
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf layerVersionArn
      `Prelude.seq` Prelude.rnf licenseInfo
      `Prelude.seq` Prelude.rnf version
