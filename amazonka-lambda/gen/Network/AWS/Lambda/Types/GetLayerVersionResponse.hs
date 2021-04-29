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
-- Module      : Network.AWS.Lambda.Types.GetLayerVersionResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.GetLayerVersionResponse where

import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.Runtime
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newGetLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { -- | The date that the layer version was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the layer.
    layerArn :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the layer version.
    layerVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Details about the layer version.
    content :: Prelude.Maybe LayerVersionContentOutput,
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Prelude.Maybe [Runtime],
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s software license.
    licenseInfo :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      layerArn = Prelude.Nothing,
      version = Prelude.Nothing,
      layerVersionArn = Prelude.Nothing,
      content = Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing,
      description = Prelude.Nothing,
      licenseInfo = Prelude.Nothing
    }

-- | The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
getLayerVersionResponse_createdDate :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_createdDate = Lens.lens (\GetLayerVersionResponse' {createdDate} -> createdDate) (\s@GetLayerVersionResponse' {} a -> s {createdDate = a} :: GetLayerVersionResponse)

-- | The ARN of the layer.
getLayerVersionResponse_layerArn :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_layerArn = Lens.lens (\GetLayerVersionResponse' {layerArn} -> layerArn) (\s@GetLayerVersionResponse' {} a -> s {layerArn = a} :: GetLayerVersionResponse)

-- | The version number.
getLayerVersionResponse_version :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Integer)
getLayerVersionResponse_version = Lens.lens (\GetLayerVersionResponse' {version} -> version) (\s@GetLayerVersionResponse' {} a -> s {version = a} :: GetLayerVersionResponse)

-- | The ARN of the layer version.
getLayerVersionResponse_layerVersionArn :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_layerVersionArn = Lens.lens (\GetLayerVersionResponse' {layerVersionArn} -> layerVersionArn) (\s@GetLayerVersionResponse' {} a -> s {layerVersionArn = a} :: GetLayerVersionResponse)

-- | Details about the layer version.
getLayerVersionResponse_content :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe LayerVersionContentOutput)
getLayerVersionResponse_content = Lens.lens (\GetLayerVersionResponse' {content} -> content) (\s@GetLayerVersionResponse' {} a -> s {content = a} :: GetLayerVersionResponse)

-- | The layer\'s compatible runtimes.
getLayerVersionResponse_compatibleRuntimes :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe [Runtime])
getLayerVersionResponse_compatibleRuntimes = Lens.lens (\GetLayerVersionResponse' {compatibleRuntimes} -> compatibleRuntimes) (\s@GetLayerVersionResponse' {} a -> s {compatibleRuntimes = a} :: GetLayerVersionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the version.
getLayerVersionResponse_description :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_description = Lens.lens (\GetLayerVersionResponse' {description} -> description) (\s@GetLayerVersionResponse' {} a -> s {description = a} :: GetLayerVersionResponse)

-- | The layer\'s software license.
getLayerVersionResponse_licenseInfo :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_licenseInfo = Lens.lens (\GetLayerVersionResponse' {licenseInfo} -> licenseInfo) (\s@GetLayerVersionResponse' {} a -> s {licenseInfo = a} :: GetLayerVersionResponse)

instance Prelude.FromJSON GetLayerVersionResponse where
  parseJSON =
    Prelude.withObject
      "GetLayerVersionResponse"
      ( \x ->
          GetLayerVersionResponse'
            Prelude.<$> (x Prelude..:? "CreatedDate")
            Prelude.<*> (x Prelude..:? "LayerArn")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "LayerVersionArn")
            Prelude.<*> (x Prelude..:? "Content")
            Prelude.<*> ( x Prelude..:? "CompatibleRuntimes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "LicenseInfo")
      )

instance Prelude.Hashable GetLayerVersionResponse

instance Prelude.NFData GetLayerVersionResponse
