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
-- Module      : Network.AWS.ECR.Types.ImageScanFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFinding where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.FindingSeverity
import qualified Network.AWS.Lens as Lens

-- | Contains information about an image scan finding.
--
-- /See:/ 'newImageScanFinding' smart constructor.
data ImageScanFinding = ImageScanFinding'
  { -- | A link containing additional details about the security vulnerability.
    uri :: Core.Maybe Core.Text,
    -- | The finding severity.
    severity :: Core.Maybe FindingSeverity,
    -- | The name associated with the finding, usually a CVE number.
    name :: Core.Maybe Core.Text,
    -- | A collection of attributes of the host from which the finding is
    -- generated.
    attributes :: Core.Maybe [Attribute],
    -- | The description of the finding.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageScanFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'imageScanFinding_uri' - A link containing additional details about the security vulnerability.
--
-- 'severity', 'imageScanFinding_severity' - The finding severity.
--
-- 'name', 'imageScanFinding_name' - The name associated with the finding, usually a CVE number.
--
-- 'attributes', 'imageScanFinding_attributes' - A collection of attributes of the host from which the finding is
-- generated.
--
-- 'description', 'imageScanFinding_description' - The description of the finding.
newImageScanFinding ::
  ImageScanFinding
newImageScanFinding =
  ImageScanFinding'
    { uri = Core.Nothing,
      severity = Core.Nothing,
      name = Core.Nothing,
      attributes = Core.Nothing,
      description = Core.Nothing
    }

-- | A link containing additional details about the security vulnerability.
imageScanFinding_uri :: Lens.Lens' ImageScanFinding (Core.Maybe Core.Text)
imageScanFinding_uri = Lens.lens (\ImageScanFinding' {uri} -> uri) (\s@ImageScanFinding' {} a -> s {uri = a} :: ImageScanFinding)

-- | The finding severity.
imageScanFinding_severity :: Lens.Lens' ImageScanFinding (Core.Maybe FindingSeverity)
imageScanFinding_severity = Lens.lens (\ImageScanFinding' {severity} -> severity) (\s@ImageScanFinding' {} a -> s {severity = a} :: ImageScanFinding)

-- | The name associated with the finding, usually a CVE number.
imageScanFinding_name :: Lens.Lens' ImageScanFinding (Core.Maybe Core.Text)
imageScanFinding_name = Lens.lens (\ImageScanFinding' {name} -> name) (\s@ImageScanFinding' {} a -> s {name = a} :: ImageScanFinding)

-- | A collection of attributes of the host from which the finding is
-- generated.
imageScanFinding_attributes :: Lens.Lens' ImageScanFinding (Core.Maybe [Attribute])
imageScanFinding_attributes = Lens.lens (\ImageScanFinding' {attributes} -> attributes) (\s@ImageScanFinding' {} a -> s {attributes = a} :: ImageScanFinding) Core.. Lens.mapping Lens._Coerce

-- | The description of the finding.
imageScanFinding_description :: Lens.Lens' ImageScanFinding (Core.Maybe Core.Text)
imageScanFinding_description = Lens.lens (\ImageScanFinding' {description} -> description) (\s@ImageScanFinding' {} a -> s {description = a} :: ImageScanFinding)

instance Core.FromJSON ImageScanFinding where
  parseJSON =
    Core.withObject
      "ImageScanFinding"
      ( \x ->
          ImageScanFinding'
            Core.<$> (x Core..:? "uri")
            Core.<*> (x Core..:? "severity")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable ImageScanFinding

instance Core.NFData ImageScanFinding
