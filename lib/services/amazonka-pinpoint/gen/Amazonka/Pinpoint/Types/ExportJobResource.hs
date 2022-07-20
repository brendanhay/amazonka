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
-- Module      : Amazonka.Pinpoint.Types.ExportJobResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ExportJobResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the resource settings for a job that exports
-- endpoint definitions to a file. The file can be added directly to an
-- Amazon Simple Storage Service (Amazon S3) bucket by using the Amazon
-- Pinpoint API or downloaded directly to a computer by using the Amazon
-- Pinpoint console.
--
-- /See:/ 'newExportJobResource' smart constructor.
data ExportJobResource = ExportJobResource'
  { -- | The version of the segment that the endpoint definitions were exported
    -- from.
    segmentVersion :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the segment that the endpoint definitions were
    -- exported from. If this value isn\'t present, Amazon Pinpoint exported
    -- definitions for all the endpoints that are associated with the
    -- application.
    segmentId :: Prelude.Maybe Prelude.Text,
    -- | The URL of the location in an Amazon Simple Storage Service (Amazon S3)
    -- bucket where the endpoint definitions were exported to. This location is
    -- typically a folder that contains multiple files. The URL should be in
    -- the following format: s3:\/\/bucket-name\/folder-name\/.
    s3UrlPrefix :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorized Amazon Pinpoint to access the Amazon S3
    -- location where the endpoint definitions were exported to.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJobResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentVersion', 'exportJobResource_segmentVersion' - The version of the segment that the endpoint definitions were exported
-- from.
--
-- 'segmentId', 'exportJobResource_segmentId' - The identifier for the segment that the endpoint definitions were
-- exported from. If this value isn\'t present, Amazon Pinpoint exported
-- definitions for all the endpoints that are associated with the
-- application.
--
-- 's3UrlPrefix', 'exportJobResource_s3UrlPrefix' - The URL of the location in an Amazon Simple Storage Service (Amazon S3)
-- bucket where the endpoint definitions were exported to. This location is
-- typically a folder that contains multiple files. The URL should be in
-- the following format: s3:\/\/bucket-name\/folder-name\/.
--
-- 'roleArn', 'exportJobResource_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorized Amazon Pinpoint to access the Amazon S3
-- location where the endpoint definitions were exported to.
newExportJobResource ::
  -- | 's3UrlPrefix'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  ExportJobResource
newExportJobResource pS3UrlPrefix_ pRoleArn_ =
  ExportJobResource'
    { segmentVersion =
        Prelude.Nothing,
      segmentId = Prelude.Nothing,
      s3UrlPrefix = pS3UrlPrefix_,
      roleArn = pRoleArn_
    }

-- | The version of the segment that the endpoint definitions were exported
-- from.
exportJobResource_segmentVersion :: Lens.Lens' ExportJobResource (Prelude.Maybe Prelude.Int)
exportJobResource_segmentVersion = Lens.lens (\ExportJobResource' {segmentVersion} -> segmentVersion) (\s@ExportJobResource' {} a -> s {segmentVersion = a} :: ExportJobResource)

-- | The identifier for the segment that the endpoint definitions were
-- exported from. If this value isn\'t present, Amazon Pinpoint exported
-- definitions for all the endpoints that are associated with the
-- application.
exportJobResource_segmentId :: Lens.Lens' ExportJobResource (Prelude.Maybe Prelude.Text)
exportJobResource_segmentId = Lens.lens (\ExportJobResource' {segmentId} -> segmentId) (\s@ExportJobResource' {} a -> s {segmentId = a} :: ExportJobResource)

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3)
-- bucket where the endpoint definitions were exported to. This location is
-- typically a folder that contains multiple files. The URL should be in
-- the following format: s3:\/\/bucket-name\/folder-name\/.
exportJobResource_s3UrlPrefix :: Lens.Lens' ExportJobResource Prelude.Text
exportJobResource_s3UrlPrefix = Lens.lens (\ExportJobResource' {s3UrlPrefix} -> s3UrlPrefix) (\s@ExportJobResource' {} a -> s {s3UrlPrefix = a} :: ExportJobResource)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorized Amazon Pinpoint to access the Amazon S3
-- location where the endpoint definitions were exported to.
exportJobResource_roleArn :: Lens.Lens' ExportJobResource Prelude.Text
exportJobResource_roleArn = Lens.lens (\ExportJobResource' {roleArn} -> roleArn) (\s@ExportJobResource' {} a -> s {roleArn = a} :: ExportJobResource)

instance Core.FromJSON ExportJobResource where
  parseJSON =
    Core.withObject
      "ExportJobResource"
      ( \x ->
          ExportJobResource'
            Prelude.<$> (x Core..:? "SegmentVersion")
            Prelude.<*> (x Core..:? "SegmentId")
            Prelude.<*> (x Core..: "S3UrlPrefix")
            Prelude.<*> (x Core..: "RoleArn")
      )

instance Prelude.Hashable ExportJobResource where
  hashWithSalt _salt ExportJobResource' {..} =
    _salt `Prelude.hashWithSalt` segmentVersion
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` s3UrlPrefix
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData ExportJobResource where
  rnf ExportJobResource' {..} =
    Prelude.rnf segmentVersion
      `Prelude.seq` Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf s3UrlPrefix
      `Prelude.seq` Prelude.rnf roleArn
