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
-- Module      : Network.AWS.Pinpoint.Types.ExportJobRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the settings for a job that exports endpoint definitions to an
-- Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'newExportJobRequest' smart constructor.
data ExportJobRequest = ExportJobRequest'
  { -- | The version of the segment to export endpoint definitions from, if
    -- specified.
    segmentVersion :: Core.Maybe Core.Int,
    -- | The identifier for the segment to export endpoint definitions from. If
    -- you don\'t specify this value, Amazon Pinpoint exports definitions for
    -- all the endpoints that are associated with the application.
    segmentId :: Core.Maybe Core.Text,
    -- | The URL of the location in an Amazon Simple Storage Service (Amazon S3)
    -- bucket where you want to export endpoint definitions to. This location
    -- is typically a folder that contains multiple files. The URL should be in
    -- the following format: s3:\/\/bucket-name\/folder-name\/.
    s3UrlPrefix :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
    -- location where you want to export endpoint definitions to.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportJobRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentVersion', 'exportJobRequest_segmentVersion' - The version of the segment to export endpoint definitions from, if
-- specified.
--
-- 'segmentId', 'exportJobRequest_segmentId' - The identifier for the segment to export endpoint definitions from. If
-- you don\'t specify this value, Amazon Pinpoint exports definitions for
-- all the endpoints that are associated with the application.
--
-- 's3UrlPrefix', 'exportJobRequest_s3UrlPrefix' - The URL of the location in an Amazon Simple Storage Service (Amazon S3)
-- bucket where you want to export endpoint definitions to. This location
-- is typically a folder that contains multiple files. The URL should be in
-- the following format: s3:\/\/bucket-name\/folder-name\/.
--
-- 'roleArn', 'exportJobRequest_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location where you want to export endpoint definitions to.
newExportJobRequest ::
  -- | 's3UrlPrefix'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  ExportJobRequest
newExportJobRequest pS3UrlPrefix_ pRoleArn_ =
  ExportJobRequest'
    { segmentVersion = Core.Nothing,
      segmentId = Core.Nothing,
      s3UrlPrefix = pS3UrlPrefix_,
      roleArn = pRoleArn_
    }

-- | The version of the segment to export endpoint definitions from, if
-- specified.
exportJobRequest_segmentVersion :: Lens.Lens' ExportJobRequest (Core.Maybe Core.Int)
exportJobRequest_segmentVersion = Lens.lens (\ExportJobRequest' {segmentVersion} -> segmentVersion) (\s@ExportJobRequest' {} a -> s {segmentVersion = a} :: ExportJobRequest)

-- | The identifier for the segment to export endpoint definitions from. If
-- you don\'t specify this value, Amazon Pinpoint exports definitions for
-- all the endpoints that are associated with the application.
exportJobRequest_segmentId :: Lens.Lens' ExportJobRequest (Core.Maybe Core.Text)
exportJobRequest_segmentId = Lens.lens (\ExportJobRequest' {segmentId} -> segmentId) (\s@ExportJobRequest' {} a -> s {segmentId = a} :: ExportJobRequest)

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3)
-- bucket where you want to export endpoint definitions to. This location
-- is typically a folder that contains multiple files. The URL should be in
-- the following format: s3:\/\/bucket-name\/folder-name\/.
exportJobRequest_s3UrlPrefix :: Lens.Lens' ExportJobRequest Core.Text
exportJobRequest_s3UrlPrefix = Lens.lens (\ExportJobRequest' {s3UrlPrefix} -> s3UrlPrefix) (\s@ExportJobRequest' {} a -> s {s3UrlPrefix = a} :: ExportJobRequest)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location where you want to export endpoint definitions to.
exportJobRequest_roleArn :: Lens.Lens' ExportJobRequest Core.Text
exportJobRequest_roleArn = Lens.lens (\ExportJobRequest' {roleArn} -> roleArn) (\s@ExportJobRequest' {} a -> s {roleArn = a} :: ExportJobRequest)

instance Core.Hashable ExportJobRequest

instance Core.NFData ExportJobRequest

instance Core.ToJSON ExportJobRequest where
  toJSON ExportJobRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SegmentVersion" Core..=) Core.<$> segmentVersion,
            ("SegmentId" Core..=) Core.<$> segmentId,
            Core.Just ("S3UrlPrefix" Core..= s3UrlPrefix),
            Core.Just ("RoleArn" Core..= roleArn)
          ]
      )
