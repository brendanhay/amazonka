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
-- Module      : Network.AWS.CodeStar.Types.S3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.S3Location where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon S3 location where the source code files provided with the
-- project request are stored.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The Amazon S3 bucket name where the source code files provided with the
    -- project request are stored.
    bucketName :: Core.Maybe Core.Text,
    -- | The Amazon S3 object key where the source code files provided with the
    -- project request are stored.
    bucketKey :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3Location_bucketName' - The Amazon S3 bucket name where the source code files provided with the
-- project request are stored.
--
-- 'bucketKey', 's3Location_bucketKey' - The Amazon S3 object key where the source code files provided with the
-- project request are stored.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { bucketName = Core.Nothing,
      bucketKey = Core.Nothing
    }

-- | The Amazon S3 bucket name where the source code files provided with the
-- project request are stored.
s3Location_bucketName :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_bucketName = Lens.lens (\S3Location' {bucketName} -> bucketName) (\s@S3Location' {} a -> s {bucketName = a} :: S3Location)

-- | The Amazon S3 object key where the source code files provided with the
-- project request are stored.
s3Location_bucketKey :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_bucketKey = Lens.lens (\S3Location' {bucketKey} -> bucketKey) (\s@S3Location' {} a -> s {bucketKey = a} :: S3Location)

instance Core.Hashable S3Location

instance Core.NFData S3Location

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Core.catMaybes
          [ ("bucketName" Core..=) Core.<$> bucketName,
            ("bucketKey" Core..=) Core.<$> bucketKey
          ]
      )
