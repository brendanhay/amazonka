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
-- Module      : Network.AWS.Glue.Types.S3Target
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Target where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies a data store in Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'newS3Target' smart constructor.
data S3Target = S3Target'
  { -- | The name of a connection which allows a job or crawler to access data in
    -- Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon
    -- VPC).
    connectionName :: Core.Maybe Core.Text,
    -- | A list of glob patterns used to exclude from the crawl. For more
    -- information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
    exclusions :: Core.Maybe [Core.Text],
    -- | The path to the Amazon S3 target.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 's3Target_connectionName' - The name of a connection which allows a job or crawler to access data in
-- Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon
-- VPC).
--
-- 'exclusions', 's3Target_exclusions' - A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
--
-- 'path', 's3Target_path' - The path to the Amazon S3 target.
newS3Target ::
  S3Target
newS3Target =
  S3Target'
    { connectionName = Core.Nothing,
      exclusions = Core.Nothing,
      path = Core.Nothing
    }

-- | The name of a connection which allows a job or crawler to access data in
-- Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon
-- VPC).
s3Target_connectionName :: Lens.Lens' S3Target (Core.Maybe Core.Text)
s3Target_connectionName = Lens.lens (\S3Target' {connectionName} -> connectionName) (\s@S3Target' {} a -> s {connectionName = a} :: S3Target)

-- | A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
s3Target_exclusions :: Lens.Lens' S3Target (Core.Maybe [Core.Text])
s3Target_exclusions = Lens.lens (\S3Target' {exclusions} -> exclusions) (\s@S3Target' {} a -> s {exclusions = a} :: S3Target) Core.. Lens.mapping Lens._Coerce

-- | The path to the Amazon S3 target.
s3Target_path :: Lens.Lens' S3Target (Core.Maybe Core.Text)
s3Target_path = Lens.lens (\S3Target' {path} -> path) (\s@S3Target' {} a -> s {path = a} :: S3Target)

instance Core.FromJSON S3Target where
  parseJSON =
    Core.withObject
      "S3Target"
      ( \x ->
          S3Target'
            Core.<$> (x Core..:? "ConnectionName")
            Core.<*> (x Core..:? "Exclusions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Path")
      )

instance Core.Hashable S3Target

instance Core.NFData S3Target

instance Core.ToJSON S3Target where
  toJSON S3Target' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionName" Core..=) Core.<$> connectionName,
            ("Exclusions" Core..=) Core.<$> exclusions,
            ("Path" Core..=) Core.<$> path
          ]
      )
