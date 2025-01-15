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
-- Module      : Amazonka.Lambda.Types.GetLayerVersionResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.GetLayerVersionResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.Architecture
import Amazonka.Lambda.Types.LayerVersionContentOutput
import Amazonka.Lambda.Types.Runtime
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newGetLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
    compatibleArchitectures :: Prelude.Maybe [Architecture],
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Prelude.Maybe [Runtime],
    -- | Details about the layer version.
    content :: Prelude.Maybe LayerVersionContentOutput,
    -- | The date that the layer version was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the layer.
    layerArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the layer version.
    layerVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s software license.
    licenseInfo :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer
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
-- 'compatibleArchitectures', 'getLayerVersionResponse_compatibleArchitectures' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
--
-- 'compatibleRuntimes', 'getLayerVersionResponse_compatibleRuntimes' - The layer\'s compatible runtimes.
--
-- 'content', 'getLayerVersionResponse_content' - Details about the layer version.
--
-- 'createdDate', 'getLayerVersionResponse_createdDate' - The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'description', 'getLayerVersionResponse_description' - The description of the version.
--
-- 'layerArn', 'getLayerVersionResponse_layerArn' - The ARN of the layer.
--
-- 'layerVersionArn', 'getLayerVersionResponse_layerVersionArn' - The ARN of the layer version.
--
-- 'licenseInfo', 'getLayerVersionResponse_licenseInfo' - The layer\'s software license.
--
-- 'version', 'getLayerVersionResponse_version' - The version number.
newGetLayerVersionResponse ::
  GetLayerVersionResponse
newGetLayerVersionResponse =
  GetLayerVersionResponse'
    { compatibleArchitectures =
        Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing,
      content = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      layerArn = Prelude.Nothing,
      layerVersionArn = Prelude.Nothing,
      licenseInfo = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
getLayerVersionResponse_compatibleArchitectures :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe [Architecture])
getLayerVersionResponse_compatibleArchitectures = Lens.lens (\GetLayerVersionResponse' {compatibleArchitectures} -> compatibleArchitectures) (\s@GetLayerVersionResponse' {} a -> s {compatibleArchitectures = a} :: GetLayerVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The layer\'s compatible runtimes.
getLayerVersionResponse_compatibleRuntimes :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe [Runtime])
getLayerVersionResponse_compatibleRuntimes = Lens.lens (\GetLayerVersionResponse' {compatibleRuntimes} -> compatibleRuntimes) (\s@GetLayerVersionResponse' {} a -> s {compatibleRuntimes = a} :: GetLayerVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Details about the layer version.
getLayerVersionResponse_content :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe LayerVersionContentOutput)
getLayerVersionResponse_content = Lens.lens (\GetLayerVersionResponse' {content} -> content) (\s@GetLayerVersionResponse' {} a -> s {content = a} :: GetLayerVersionResponse)

-- | The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
getLayerVersionResponse_createdDate :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_createdDate = Lens.lens (\GetLayerVersionResponse' {createdDate} -> createdDate) (\s@GetLayerVersionResponse' {} a -> s {createdDate = a} :: GetLayerVersionResponse)

-- | The description of the version.
getLayerVersionResponse_description :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_description = Lens.lens (\GetLayerVersionResponse' {description} -> description) (\s@GetLayerVersionResponse' {} a -> s {description = a} :: GetLayerVersionResponse)

-- | The ARN of the layer.
getLayerVersionResponse_layerArn :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_layerArn = Lens.lens (\GetLayerVersionResponse' {layerArn} -> layerArn) (\s@GetLayerVersionResponse' {} a -> s {layerArn = a} :: GetLayerVersionResponse)

-- | The ARN of the layer version.
getLayerVersionResponse_layerVersionArn :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_layerVersionArn = Lens.lens (\GetLayerVersionResponse' {layerVersionArn} -> layerVersionArn) (\s@GetLayerVersionResponse' {} a -> s {layerVersionArn = a} :: GetLayerVersionResponse)

-- | The layer\'s software license.
getLayerVersionResponse_licenseInfo :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Text)
getLayerVersionResponse_licenseInfo = Lens.lens (\GetLayerVersionResponse' {licenseInfo} -> licenseInfo) (\s@GetLayerVersionResponse' {} a -> s {licenseInfo = a} :: GetLayerVersionResponse)

-- | The version number.
getLayerVersionResponse_version :: Lens.Lens' GetLayerVersionResponse (Prelude.Maybe Prelude.Integer)
getLayerVersionResponse_version = Lens.lens (\GetLayerVersionResponse' {version} -> version) (\s@GetLayerVersionResponse' {} a -> s {version = a} :: GetLayerVersionResponse)

instance Data.FromJSON GetLayerVersionResponse where
  parseJSON =
    Data.withObject
      "GetLayerVersionResponse"
      ( \x ->
          GetLayerVersionResponse'
            Prelude.<$> ( x
                            Data..:? "CompatibleArchitectures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "CompatibleRuntimes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LayerArn")
            Prelude.<*> (x Data..:? "LayerVersionArn")
            Prelude.<*> (x Data..:? "LicenseInfo")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable GetLayerVersionResponse where
  hashWithSalt _salt GetLayerVersionResponse' {..} =
    _salt
      `Prelude.hashWithSalt` compatibleArchitectures
      `Prelude.hashWithSalt` compatibleRuntimes
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` layerArn
      `Prelude.hashWithSalt` layerVersionArn
      `Prelude.hashWithSalt` licenseInfo
      `Prelude.hashWithSalt` version

instance Prelude.NFData GetLayerVersionResponse where
  rnf GetLayerVersionResponse' {..} =
    Prelude.rnf compatibleArchitectures `Prelude.seq`
      Prelude.rnf compatibleRuntimes `Prelude.seq`
        Prelude.rnf content `Prelude.seq`
          Prelude.rnf createdDate `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf layerArn `Prelude.seq`
                Prelude.rnf layerVersionArn `Prelude.seq`
                  Prelude.rnf licenseInfo `Prelude.seq`
                    Prelude.rnf version
