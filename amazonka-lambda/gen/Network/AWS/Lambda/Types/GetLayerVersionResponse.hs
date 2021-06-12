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
-- Module      : Network.AWS.Lambda.Types.GetLayerVersionResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.GetLayerVersionResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newGetLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { -- | The date that the layer version was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Core.Maybe Core.Text,
    -- | The ARN of the layer.
    layerArn :: Core.Maybe Core.Text,
    -- | The version number.
    version :: Core.Maybe Core.Integer,
    -- | The ARN of the layer version.
    layerVersionArn :: Core.Maybe Core.Text,
    -- | Details about the layer version.
    content :: Core.Maybe LayerVersionContentOutput,
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Core.Maybe [Runtime],
    -- | The description of the version.
    description :: Core.Maybe Core.Text,
    -- | The layer\'s software license.
    licenseInfo :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLayerVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'getLayerVersionResponse_createdDate' - The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'layerArn', 'getLayerVersionResponse_layerArn' - The ARN of the layer.
--
-- 'version', 'getLayerVersionResponse_version' - The version number.
--
-- 'layerVersionArn', 'getLayerVersionResponse_layerVersionArn' - The ARN of the layer version.
--
-- 'content', 'getLayerVersionResponse_content' - Details about the layer version.
--
-- 'compatibleRuntimes', 'getLayerVersionResponse_compatibleRuntimes' - The layer\'s compatible runtimes.
--
-- 'description', 'getLayerVersionResponse_description' - The description of the version.
--
-- 'licenseInfo', 'getLayerVersionResponse_licenseInfo' - The layer\'s software license.
newGetLayerVersionResponse ::
  GetLayerVersionResponse
newGetLayerVersionResponse =
  GetLayerVersionResponse'
    { createdDate =
        Core.Nothing,
      layerArn = Core.Nothing,
      version = Core.Nothing,
      layerVersionArn = Core.Nothing,
      content = Core.Nothing,
      compatibleRuntimes = Core.Nothing,
      description = Core.Nothing,
      licenseInfo = Core.Nothing
    }

-- | The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
getLayerVersionResponse_createdDate :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Text)
getLayerVersionResponse_createdDate = Lens.lens (\GetLayerVersionResponse' {createdDate} -> createdDate) (\s@GetLayerVersionResponse' {} a -> s {createdDate = a} :: GetLayerVersionResponse)

-- | The ARN of the layer.
getLayerVersionResponse_layerArn :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Text)
getLayerVersionResponse_layerArn = Lens.lens (\GetLayerVersionResponse' {layerArn} -> layerArn) (\s@GetLayerVersionResponse' {} a -> s {layerArn = a} :: GetLayerVersionResponse)

-- | The version number.
getLayerVersionResponse_version :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Integer)
getLayerVersionResponse_version = Lens.lens (\GetLayerVersionResponse' {version} -> version) (\s@GetLayerVersionResponse' {} a -> s {version = a} :: GetLayerVersionResponse)

-- | The ARN of the layer version.
getLayerVersionResponse_layerVersionArn :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Text)
getLayerVersionResponse_layerVersionArn = Lens.lens (\GetLayerVersionResponse' {layerVersionArn} -> layerVersionArn) (\s@GetLayerVersionResponse' {} a -> s {layerVersionArn = a} :: GetLayerVersionResponse)

-- | Details about the layer version.
getLayerVersionResponse_content :: Lens.Lens' GetLayerVersionResponse (Core.Maybe LayerVersionContentOutput)
getLayerVersionResponse_content = Lens.lens (\GetLayerVersionResponse' {content} -> content) (\s@GetLayerVersionResponse' {} a -> s {content = a} :: GetLayerVersionResponse)

-- | The layer\'s compatible runtimes.
getLayerVersionResponse_compatibleRuntimes :: Lens.Lens' GetLayerVersionResponse (Core.Maybe [Runtime])
getLayerVersionResponse_compatibleRuntimes = Lens.lens (\GetLayerVersionResponse' {compatibleRuntimes} -> compatibleRuntimes) (\s@GetLayerVersionResponse' {} a -> s {compatibleRuntimes = a} :: GetLayerVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | The description of the version.
getLayerVersionResponse_description :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Text)
getLayerVersionResponse_description = Lens.lens (\GetLayerVersionResponse' {description} -> description) (\s@GetLayerVersionResponse' {} a -> s {description = a} :: GetLayerVersionResponse)

-- | The layer\'s software license.
getLayerVersionResponse_licenseInfo :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Text)
getLayerVersionResponse_licenseInfo = Lens.lens (\GetLayerVersionResponse' {licenseInfo} -> licenseInfo) (\s@GetLayerVersionResponse' {} a -> s {licenseInfo = a} :: GetLayerVersionResponse)

instance Core.FromJSON GetLayerVersionResponse where
  parseJSON =
    Core.withObject
      "GetLayerVersionResponse"
      ( \x ->
          GetLayerVersionResponse'
            Core.<$> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "LayerArn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "LayerVersionArn")
            Core.<*> (x Core..:? "Content")
            Core.<*> ( x Core..:? "CompatibleRuntimes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "LicenseInfo")
      )

instance Core.Hashable GetLayerVersionResponse

instance Core.NFData GetLayerVersionResponse
