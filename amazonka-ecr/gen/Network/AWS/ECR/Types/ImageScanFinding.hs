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
-- Module      : Network.AWS.ECR.Types.ImageScanFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFinding where

import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.FindingSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an image scan finding.
--
-- /See:/ 'newImageScanFinding' smart constructor.
data ImageScanFinding = ImageScanFinding'
  { -- | A link containing additional details about the security vulnerability.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The finding severity.
    severity :: Prelude.Maybe FindingSeverity,
    -- | The name associated with the finding, usually a CVE number.
    name :: Prelude.Maybe Prelude.Text,
    -- | A collection of attributes of the host from which the finding is
    -- generated.
    attributes :: Prelude.Maybe [Attribute],
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { uri = Prelude.Nothing,
      severity = Prelude.Nothing,
      name = Prelude.Nothing,
      attributes = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A link containing additional details about the security vulnerability.
imageScanFinding_uri :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_uri = Lens.lens (\ImageScanFinding' {uri} -> uri) (\s@ImageScanFinding' {} a -> s {uri = a} :: ImageScanFinding)

-- | The finding severity.
imageScanFinding_severity :: Lens.Lens' ImageScanFinding (Prelude.Maybe FindingSeverity)
imageScanFinding_severity = Lens.lens (\ImageScanFinding' {severity} -> severity) (\s@ImageScanFinding' {} a -> s {severity = a} :: ImageScanFinding)

-- | The name associated with the finding, usually a CVE number.
imageScanFinding_name :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_name = Lens.lens (\ImageScanFinding' {name} -> name) (\s@ImageScanFinding' {} a -> s {name = a} :: ImageScanFinding)

-- | A collection of attributes of the host from which the finding is
-- generated.
imageScanFinding_attributes :: Lens.Lens' ImageScanFinding (Prelude.Maybe [Attribute])
imageScanFinding_attributes = Lens.lens (\ImageScanFinding' {attributes} -> attributes) (\s@ImageScanFinding' {} a -> s {attributes = a} :: ImageScanFinding) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the finding.
imageScanFinding_description :: Lens.Lens' ImageScanFinding (Prelude.Maybe Prelude.Text)
imageScanFinding_description = Lens.lens (\ImageScanFinding' {description} -> description) (\s@ImageScanFinding' {} a -> s {description = a} :: ImageScanFinding)

instance Prelude.FromJSON ImageScanFinding where
  parseJSON =
    Prelude.withObject
      "ImageScanFinding"
      ( \x ->
          ImageScanFinding'
            Prelude.<$> (x Prelude..:? "uri")
            Prelude.<*> (x Prelude..:? "severity")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> ( x Prelude..:? "attributes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable ImageScanFinding

instance Prelude.NFData ImageScanFinding
