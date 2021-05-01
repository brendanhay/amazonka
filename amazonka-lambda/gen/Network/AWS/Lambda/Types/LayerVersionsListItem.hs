{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
--
-- /See:/ 'newLayerVersionsListItem' smart constructor.
data LayerVersionsListItem = LayerVersionsListItem'
  { -- | The date that the version was created, in ISO 8601 format. For example,
    -- @2018-11-27T15:10:45.123+0000@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the layer version.
    layerVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Prelude.Maybe [Runtime],
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s open-source license.
    licenseInfo :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createdDate =
        Prelude.Nothing,
      version = Prelude.Nothing,
      layerVersionArn = Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing,
      description = Prelude.Nothing,
      licenseInfo = Prelude.Nothing
    }

-- | The date that the version was created, in ISO 8601 format. For example,
-- @2018-11-27T15:10:45.123+0000@.
layerVersionsListItem_createdDate :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_createdDate = Lens.lens (\LayerVersionsListItem' {createdDate} -> createdDate) (\s@LayerVersionsListItem' {} a -> s {createdDate = a} :: LayerVersionsListItem)

-- | The version number.
layerVersionsListItem_version :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Integer)
layerVersionsListItem_version = Lens.lens (\LayerVersionsListItem' {version} -> version) (\s@LayerVersionsListItem' {} a -> s {version = a} :: LayerVersionsListItem)

-- | The ARN of the layer version.
layerVersionsListItem_layerVersionArn :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_layerVersionArn = Lens.lens (\LayerVersionsListItem' {layerVersionArn} -> layerVersionArn) (\s@LayerVersionsListItem' {} a -> s {layerVersionArn = a} :: LayerVersionsListItem)

-- | The layer\'s compatible runtimes.
layerVersionsListItem_compatibleRuntimes :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe [Runtime])
layerVersionsListItem_compatibleRuntimes = Lens.lens (\LayerVersionsListItem' {compatibleRuntimes} -> compatibleRuntimes) (\s@LayerVersionsListItem' {} a -> s {compatibleRuntimes = a} :: LayerVersionsListItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the version.
layerVersionsListItem_description :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_description = Lens.lens (\LayerVersionsListItem' {description} -> description) (\s@LayerVersionsListItem' {} a -> s {description = a} :: LayerVersionsListItem)

-- | The layer\'s open-source license.
layerVersionsListItem_licenseInfo :: Lens.Lens' LayerVersionsListItem (Prelude.Maybe Prelude.Text)
layerVersionsListItem_licenseInfo = Lens.lens (\LayerVersionsListItem' {licenseInfo} -> licenseInfo) (\s@LayerVersionsListItem' {} a -> s {licenseInfo = a} :: LayerVersionsListItem)

instance Prelude.FromJSON LayerVersionsListItem where
  parseJSON =
    Prelude.withObject
      "LayerVersionsListItem"
      ( \x ->
          LayerVersionsListItem'
            Prelude.<$> (x Prelude..:? "CreatedDate")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "LayerVersionArn")
            Prelude.<*> ( x Prelude..:? "CompatibleRuntimes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "LicenseInfo")
      )

instance Prelude.Hashable LayerVersionsListItem

instance Prelude.NFData LayerVersionsListItem
