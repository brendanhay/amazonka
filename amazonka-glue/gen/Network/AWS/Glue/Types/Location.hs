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
-- Module      : Network.AWS.Glue.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Location where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CodeGenNodeArg
import qualified Network.AWS.Lens as Lens

-- | The location of resources.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | A JDBC location.
    jdbc :: Core.Maybe [CodeGenNodeArg],
    -- | An Amazon DynamoDB table location.
    dynamoDB :: Core.Maybe [CodeGenNodeArg],
    -- | An Amazon Simple Storage Service (Amazon S3) location.
    s3 :: Core.Maybe [CodeGenNodeArg]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jdbc', 'location_jdbc' - A JDBC location.
--
-- 'dynamoDB', 'location_dynamoDB' - An Amazon DynamoDB table location.
--
-- 's3', 'location_s3' - An Amazon Simple Storage Service (Amazon S3) location.
newLocation ::
  Location
newLocation =
  Location'
    { jdbc = Core.Nothing,
      dynamoDB = Core.Nothing,
      s3 = Core.Nothing
    }

-- | A JDBC location.
location_jdbc :: Lens.Lens' Location (Core.Maybe [CodeGenNodeArg])
location_jdbc = Lens.lens (\Location' {jdbc} -> jdbc) (\s@Location' {} a -> s {jdbc = a} :: Location) Core.. Lens.mapping Lens._Coerce

-- | An Amazon DynamoDB table location.
location_dynamoDB :: Lens.Lens' Location (Core.Maybe [CodeGenNodeArg])
location_dynamoDB = Lens.lens (\Location' {dynamoDB} -> dynamoDB) (\s@Location' {} a -> s {dynamoDB = a} :: Location) Core.. Lens.mapping Lens._Coerce

-- | An Amazon Simple Storage Service (Amazon S3) location.
location_s3 :: Lens.Lens' Location (Core.Maybe [CodeGenNodeArg])
location_s3 = Lens.lens (\Location' {s3} -> s3) (\s@Location' {} a -> s {s3 = a} :: Location) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable Location

instance Core.NFData Location

instance Core.ToJSON Location where
  toJSON Location' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Jdbc" Core..=) Core.<$> jdbc,
            ("DynamoDB" Core..=) Core.<$> dynamoDB,
            ("S3" Core..=) Core.<$> s3
          ]
      )
