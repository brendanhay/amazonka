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
import Network.AWS.Lambda.Types.Architecture
import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newGetLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { -- | The ARN of the layer version.
    layerVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Details about the layer version.
    content :: Prelude.Maybe LayerVersionContentOutput,
    -- | The date that the layer version was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The layer\'s software license.
    licenseInfo :: Prelude.Maybe Prelude.Text,
    -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
    compatibleArchitectures :: Prelude.Maybe [Architecture],
    -- | The ARN of the layer.
    layerArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Prelude.Maybe [Runtime]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLayerVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerVersionArn', 'getLayerVersionResponse_layerVersionArn' - The ARN of the layer version.
--
-- 'content', 'getLayerVersionResponse_content' - Details about the layer version.
--
-- 'createdDate', 'getLayerVersionResponse_createdDate' - The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'version', 'getLayerVersionResponse_version' - The version number.
--
-- 'licenseInfo', 'getLayerVersionResponse_licenseInfo' - The layer\'s software license.
--
-- 'compatibleArchitectures', 'getLayerVersionResponse_compatibleArchitectures' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
--
-- 'layerArn', 'getLayerVersionResponse_layerArn' - The ARN of the layer.
--
-- 'description', 'getLayerVersionResponse_description' - The description of the version.
--
-- 'compatibleRuntimes', 'getLayerVersionResponse_compatibleRuntimes' - The layer\'s compatible runtimes.
newGetLayerVersionResponse ::
  GetLayerVersionResponse
newGetLayerVersionResponse =
  GetLayerVersionResponse'
    { layerVersionArn =
        Prelude.Nothing,
      content = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      version = Prelude.Nothing,
      licenseInfo = Prelude.Nothing,
      compatibleArchitectures = Prelude.Nothing,
      layerArn = Prelude.Nothing,
      description = Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing
    }

-- | The ARN of the layer version.
getLayerVersionResponse_layerVersionArn :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_layerVersionArn = Lens.lens (\GetLayerVersionResponse' {layerVersionArn} -> layerVersionArn) (\s@GetLayerVersionResponse' {} a -> s {layerVersionArn = a} :: GetLayerVersionResponse)

-- | Details about the layer version.
getLayerVersionResponse_content :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe LayerVersionContentOutput)
getLayerVersionResponse_content = Lens.lens (\GetLayerVersionResponse' {content} -> content) (\s@GetLayerVersionResponse' {} a -> s {content = a} :: GetLayerVersionResponse)

-- | The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
getLayerVersionResponse_createdDate :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_createdDate = Lens.lens (\GetLayerVersionResponse' {createdDate} -> createdDate) (\s@GetLayerVersionResponse' {} a -> s {createdDate = a} :: GetLayerVersionResponse)

-- | The version number.
getLayerVersionResponse_version :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Integer)
getLayerVersionResponse_version = Lens.lens (\GetLayerVersionResponse' {version} -> version) (\s@GetLayerVersionResponse' {} a -> s {version = a} :: GetLayerVersionResponse)

-- | The layer\'s software license.
getLayerVersionResponse_licenseInfo :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_licenseInfo = Lens.lens (\GetLayerVersionResponse' {licenseInfo} -> licenseInfo) (\s@GetLayerVersionResponse' {} a -> s {licenseInfo = a} :: GetLayerVersionResponse)

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
getLayerVersionResponse_compatibleArchitectures :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe [Architecture])
getLayerVersionResponse_compatibleArchitectures = Lens.lens (\GetLayerVersionResponse' {compatibleArchitectures} -> compatibleArchitectures) (\s@GetLayerVersionResponse' {} a -> s {compatibleArchitectures = a} :: GetLayerVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the layer.
getLayerVersionResponse_layerArn :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_layerArn = Lens.lens (\GetLayerVersionResponse' {layerArn} -> layerArn) (\s@GetLayerVersionResponse' {} a -> s {layerArn = a} :: GetLayerVersionResponse)

-- | The description of the version.
getLayerVersionResponse_description :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_description = Lens.lens (\GetLayerVersionResponse' {description} -> description) (\s@GetLayerVersionResponse' {} a -> s {description = a} :: GetLayerVersionResponse)

-- | The layer\'s compatible runtimes.
getLayerVersionResponse_compatibleRuntimes :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe [Runtime])
getLayerVersionResponse_compatibleRuntimes = Lens.lens (\GetLayerVersionResponse' {compatibleRuntimes} -> compatibleRuntimes) (\s@GetLayerVersionResponse' {} a -> s {compatibleRuntimes = a} :: GetLayerVersionResponse) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON GetLayerVersionResponse where
  parseJSON =
    Core.withObject
      "GetLayerVersionResponse"
      ( \x ->
          GetLayerVersionResponse'
            Prelude.<$> (x Core..:? "LayerVersionArn")
            Prelude.<*> (x Core..:? "Content")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "LicenseInfo")
            Prelude.<*> ( x Core..:? "CompatibleArchitectures"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LayerArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> ( x Core..:? "CompatibleRuntimes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GetLayerVersionResponse

instance Prelude.NFData GetLayerVersionResponse
