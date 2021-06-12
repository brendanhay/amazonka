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
-- Module      : Network.AWS.Lambda.Types.LayerVersionsListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionsListItem where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens

-- | Details about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
--
-- /See:/ 'newLayerVersionsListItem' smart constructor.
data LayerVersionsListItem = LayerVersionsListItem'
  { -- | The date that the version was created, in ISO 8601 format. For example,
    -- @2018-11-27T15:10:45.123+0000@.
    createdDate :: Core.Maybe Core.Text,
    -- | The version number.
    version :: Core.Maybe Core.Integer,
    -- | The ARN of the layer version.
    layerVersionArn :: Core.Maybe Core.Text,
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Core.Maybe [Runtime],
    -- | The description of the version.
    description :: Core.Maybe Core.Text,
    -- | The layer\'s open-source license.
    licenseInfo :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LayerVersionsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'layerVersionsListItem_createdDate' - The date that the version was created, in ISO 8601 format. For example,
-- @2018-11-27T15:10:45.123+0000@.
--
-- 'version', 'layerVersionsListItem_version' - The version number.
--
-- 'layerVersionArn', 'layerVersionsListItem_layerVersionArn' - The ARN of the layer version.
--
-- 'compatibleRuntimes', 'layerVersionsListItem_compatibleRuntimes' - The layer\'s compatible runtimes.
--
-- 'description', 'layerVersionsListItem_description' - The description of the version.
--
-- 'licenseInfo', 'layerVersionsListItem_licenseInfo' - The layer\'s open-source license.
newLayerVersionsListItem ::
  LayerVersionsListItem
newLayerVersionsListItem =
  LayerVersionsListItem'
    { createdDate = Core.Nothing,
      version = Core.Nothing,
      layerVersionArn = Core.Nothing,
      compatibleRuntimes = Core.Nothing,
      description = Core.Nothing,
      licenseInfo = Core.Nothing
    }

-- | The date that the version was created, in ISO 8601 format. For example,
-- @2018-11-27T15:10:45.123+0000@.
layerVersionsListItem_createdDate :: Lens.Lens' LayerVersionsListItem (Core.Maybe Core.Text)
layerVersionsListItem_createdDate = Lens.lens (\LayerVersionsListItem' {createdDate} -> createdDate) (\s@LayerVersionsListItem' {} a -> s {createdDate = a} :: LayerVersionsListItem)

-- | The version number.
layerVersionsListItem_version :: Lens.Lens' LayerVersionsListItem (Core.Maybe Core.Integer)
layerVersionsListItem_version = Lens.lens (\LayerVersionsListItem' {version} -> version) (\s@LayerVersionsListItem' {} a -> s {version = a} :: LayerVersionsListItem)

-- | The ARN of the layer version.
layerVersionsListItem_layerVersionArn :: Lens.Lens' LayerVersionsListItem (Core.Maybe Core.Text)
layerVersionsListItem_layerVersionArn = Lens.lens (\LayerVersionsListItem' {layerVersionArn} -> layerVersionArn) (\s@LayerVersionsListItem' {} a -> s {layerVersionArn = a} :: LayerVersionsListItem)

-- | The layer\'s compatible runtimes.
layerVersionsListItem_compatibleRuntimes :: Lens.Lens' LayerVersionsListItem (Core.Maybe [Runtime])
layerVersionsListItem_compatibleRuntimes = Lens.lens (\LayerVersionsListItem' {compatibleRuntimes} -> compatibleRuntimes) (\s@LayerVersionsListItem' {} a -> s {compatibleRuntimes = a} :: LayerVersionsListItem) Core.. Lens.mapping Lens._Coerce

-- | The description of the version.
layerVersionsListItem_description :: Lens.Lens' LayerVersionsListItem (Core.Maybe Core.Text)
layerVersionsListItem_description = Lens.lens (\LayerVersionsListItem' {description} -> description) (\s@LayerVersionsListItem' {} a -> s {description = a} :: LayerVersionsListItem)

-- | The layer\'s open-source license.
layerVersionsListItem_licenseInfo :: Lens.Lens' LayerVersionsListItem (Core.Maybe Core.Text)
layerVersionsListItem_licenseInfo = Lens.lens (\LayerVersionsListItem' {licenseInfo} -> licenseInfo) (\s@LayerVersionsListItem' {} a -> s {licenseInfo = a} :: LayerVersionsListItem)

instance Core.FromJSON LayerVersionsListItem where
  parseJSON =
    Core.withObject
      "LayerVersionsListItem"
      ( \x ->
          LayerVersionsListItem'
            Core.<$> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "LayerVersionArn")
            Core.<*> ( x Core..:? "CompatibleRuntimes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "LicenseInfo")
      )

instance Core.Hashable LayerVersionsListItem

instance Core.NFData LayerVersionsListItem
