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
-- Module      : Amazonka.ResilienceHub.Types.AppInputSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AppInputSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.EksSourceClusterNamespace
import Amazonka.ResilienceHub.Types.ResourceMappingType
import Amazonka.ResilienceHub.Types.TerraformSource

-- | The list of Resilience Hub application input sources.
--
-- /See:/ 'newAppInputSource' smart constructor.
data AppInputSource = AppInputSource'
  { -- | The namespace on your Amazon Elastic Kubernetes Service cluster.
    eksSourceClusterNamespace :: Prelude.Maybe EksSourceClusterNamespace,
    -- | The number of resources.
    resourceCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the input source. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the input source.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Terraform s3 state ﬁle.
    terraformSource :: Prelude.Maybe TerraformSource,
    -- | The resource type of the input source.
    importType :: ResourceMappingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInputSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eksSourceClusterNamespace', 'appInputSource_eksSourceClusterNamespace' - The namespace on your Amazon Elastic Kubernetes Service cluster.
--
-- 'resourceCount', 'appInputSource_resourceCount' - The number of resources.
--
-- 'sourceArn', 'appInputSource_sourceArn' - The Amazon Resource Name (ARN) of the input source. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'sourceName', 'appInputSource_sourceName' - The name of the input source.
--
-- 'terraformSource', 'appInputSource_terraformSource' - The name of the Terraform s3 state ﬁle.
--
-- 'importType', 'appInputSource_importType' - The resource type of the input source.
newAppInputSource ::
  -- | 'importType'
  ResourceMappingType ->
  AppInputSource
newAppInputSource pImportType_ =
  AppInputSource'
    { eksSourceClusterNamespace =
        Prelude.Nothing,
      resourceCount = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceName = Prelude.Nothing,
      terraformSource = Prelude.Nothing,
      importType = pImportType_
    }

-- | The namespace on your Amazon Elastic Kubernetes Service cluster.
appInputSource_eksSourceClusterNamespace :: Lens.Lens' AppInputSource (Prelude.Maybe EksSourceClusterNamespace)
appInputSource_eksSourceClusterNamespace = Lens.lens (\AppInputSource' {eksSourceClusterNamespace} -> eksSourceClusterNamespace) (\s@AppInputSource' {} a -> s {eksSourceClusterNamespace = a} :: AppInputSource)

-- | The number of resources.
appInputSource_resourceCount :: Lens.Lens' AppInputSource (Prelude.Maybe Prelude.Int)
appInputSource_resourceCount = Lens.lens (\AppInputSource' {resourceCount} -> resourceCount) (\s@AppInputSource' {} a -> s {resourceCount = a} :: AppInputSource)

-- | The Amazon Resource Name (ARN) of the input source. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
appInputSource_sourceArn :: Lens.Lens' AppInputSource (Prelude.Maybe Prelude.Text)
appInputSource_sourceArn = Lens.lens (\AppInputSource' {sourceArn} -> sourceArn) (\s@AppInputSource' {} a -> s {sourceArn = a} :: AppInputSource)

-- | The name of the input source.
appInputSource_sourceName :: Lens.Lens' AppInputSource (Prelude.Maybe Prelude.Text)
appInputSource_sourceName = Lens.lens (\AppInputSource' {sourceName} -> sourceName) (\s@AppInputSource' {} a -> s {sourceName = a} :: AppInputSource)

-- | The name of the Terraform s3 state ﬁle.
appInputSource_terraformSource :: Lens.Lens' AppInputSource (Prelude.Maybe TerraformSource)
appInputSource_terraformSource = Lens.lens (\AppInputSource' {terraformSource} -> terraformSource) (\s@AppInputSource' {} a -> s {terraformSource = a} :: AppInputSource)

-- | The resource type of the input source.
appInputSource_importType :: Lens.Lens' AppInputSource ResourceMappingType
appInputSource_importType = Lens.lens (\AppInputSource' {importType} -> importType) (\s@AppInputSource' {} a -> s {importType = a} :: AppInputSource)

instance Data.FromJSON AppInputSource where
  parseJSON =
    Data.withObject
      "AppInputSource"
      ( \x ->
          AppInputSource'
            Prelude.<$> (x Data..:? "eksSourceClusterNamespace")
            Prelude.<*> (x Data..:? "resourceCount")
            Prelude.<*> (x Data..:? "sourceArn")
            Prelude.<*> (x Data..:? "sourceName")
            Prelude.<*> (x Data..:? "terraformSource")
            Prelude.<*> (x Data..: "importType")
      )

instance Prelude.Hashable AppInputSource where
  hashWithSalt _salt AppInputSource' {..} =
    _salt
      `Prelude.hashWithSalt` eksSourceClusterNamespace
      `Prelude.hashWithSalt` resourceCount
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` terraformSource
      `Prelude.hashWithSalt` importType

instance Prelude.NFData AppInputSource where
  rnf AppInputSource' {..} =
    Prelude.rnf eksSourceClusterNamespace
      `Prelude.seq` Prelude.rnf resourceCount
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf terraformSource
      `Prelude.seq` Prelude.rnf importType
