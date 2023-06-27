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
-- Module      : Amazonka.Pinpoint.Types.ExportJobRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ExportJobRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a job that exports endpoint definitions to an
-- Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'newExportJobRequest' smart constructor.
data ExportJobRequest = ExportJobRequest'
  { -- | The identifier for the segment to export endpoint definitions from. If
    -- you don\'t specify this value, Amazon Pinpoint exports definitions for
    -- all the endpoints that are associated with the application.
    segmentId :: Prelude.Maybe Prelude.Text,
    -- | The version of the segment to export endpoint definitions from, if
    -- specified.
    segmentVersion :: Prelude.Maybe Prelude.Int,
    -- | The URL of the location in an Amazon Simple Storage Service (Amazon S3)
    -- bucket where you want to export endpoint definitions to. This location
    -- is typically a folder that contains multiple files. The URL should be in
    -- the following format: s3:\/\/bucket-name\/folder-name\/.
    s3UrlPrefix :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
    -- location where you want to export endpoint definitions to.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJobRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'exportJobRequest_segmentId' - The identifier for the segment to export endpoint definitions from. If
-- you don\'t specify this value, Amazon Pinpoint exports definitions for
-- all the endpoints that are associated with the application.
--
-- 'segmentVersion', 'exportJobRequest_segmentVersion' - The version of the segment to export endpoint definitions from, if
-- specified.
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
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  ExportJobRequest
newExportJobRequest pS3UrlPrefix_ pRoleArn_ =
  ExportJobRequest'
    { segmentId = Prelude.Nothing,
      segmentVersion = Prelude.Nothing,
      s3UrlPrefix = pS3UrlPrefix_,
      roleArn = pRoleArn_
    }

-- | The identifier for the segment to export endpoint definitions from. If
-- you don\'t specify this value, Amazon Pinpoint exports definitions for
-- all the endpoints that are associated with the application.
exportJobRequest_segmentId :: Lens.Lens' ExportJobRequest (Prelude.Maybe Prelude.Text)
exportJobRequest_segmentId = Lens.lens (\ExportJobRequest' {segmentId} -> segmentId) (\s@ExportJobRequest' {} a -> s {segmentId = a} :: ExportJobRequest)

-- | The version of the segment to export endpoint definitions from, if
-- specified.
exportJobRequest_segmentVersion :: Lens.Lens' ExportJobRequest (Prelude.Maybe Prelude.Int)
exportJobRequest_segmentVersion = Lens.lens (\ExportJobRequest' {segmentVersion} -> segmentVersion) (\s@ExportJobRequest' {} a -> s {segmentVersion = a} :: ExportJobRequest)

-- | The URL of the location in an Amazon Simple Storage Service (Amazon S3)
-- bucket where you want to export endpoint definitions to. This location
-- is typically a folder that contains multiple files. The URL should be in
-- the following format: s3:\/\/bucket-name\/folder-name\/.
exportJobRequest_s3UrlPrefix :: Lens.Lens' ExportJobRequest Prelude.Text
exportJobRequest_s3UrlPrefix = Lens.lens (\ExportJobRequest' {s3UrlPrefix} -> s3UrlPrefix) (\s@ExportJobRequest' {} a -> s {s3UrlPrefix = a} :: ExportJobRequest)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to access the Amazon S3
-- location where you want to export endpoint definitions to.
exportJobRequest_roleArn :: Lens.Lens' ExportJobRequest Prelude.Text
exportJobRequest_roleArn = Lens.lens (\ExportJobRequest' {roleArn} -> roleArn) (\s@ExportJobRequest' {} a -> s {roleArn = a} :: ExportJobRequest)

instance Prelude.Hashable ExportJobRequest where
  hashWithSalt _salt ExportJobRequest' {..} =
    _salt
      `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` segmentVersion
      `Prelude.hashWithSalt` s3UrlPrefix
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData ExportJobRequest where
  rnf ExportJobRequest' {..} =
    Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf segmentVersion
      `Prelude.seq` Prelude.rnf s3UrlPrefix
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON ExportJobRequest where
  toJSON ExportJobRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SegmentId" Data..=) Prelude.<$> segmentId,
            ("SegmentVersion" Data..=)
              Prelude.<$> segmentVersion,
            Prelude.Just ("S3UrlPrefix" Data..= s3UrlPrefix),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )
