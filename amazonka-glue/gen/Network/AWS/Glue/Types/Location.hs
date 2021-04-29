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
-- Module      : Network.AWS.Glue.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Location where

import Network.AWS.Glue.Types.CodeGenNodeArg
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The location of resources.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | A JDBC location.
    jdbc :: Prelude.Maybe [CodeGenNodeArg],
    -- | An Amazon DynamoDB table location.
    dynamoDB :: Prelude.Maybe [CodeGenNodeArg],
    -- | An Amazon Simple Storage Service (Amazon S3) location.
    s3 :: Prelude.Maybe [CodeGenNodeArg]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { jdbc = Prelude.Nothing,
      dynamoDB = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | A JDBC location.
location_jdbc :: Lens.Lens' Location (Prelude.Maybe [CodeGenNodeArg])
location_jdbc = Lens.lens (\Location' {jdbc} -> jdbc) (\s@Location' {} a -> s {jdbc = a} :: Location) Prelude.. Lens.mapping Prelude._Coerce

-- | An Amazon DynamoDB table location.
location_dynamoDB :: Lens.Lens' Location (Prelude.Maybe [CodeGenNodeArg])
location_dynamoDB = Lens.lens (\Location' {dynamoDB} -> dynamoDB) (\s@Location' {} a -> s {dynamoDB = a} :: Location) Prelude.. Lens.mapping Prelude._Coerce

-- | An Amazon Simple Storage Service (Amazon S3) location.
location_s3 :: Lens.Lens' Location (Prelude.Maybe [CodeGenNodeArg])
location_s3 = Lens.lens (\Location' {s3} -> s3) (\s@Location' {} a -> s {s3 = a} :: Location) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable Location

instance Prelude.NFData Location

instance Prelude.ToJSON Location where
  toJSON Location' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Jdbc" Prelude..=) Prelude.<$> jdbc,
            ("DynamoDB" Prelude..=) Prelude.<$> dynamoDB,
            ("S3" Prelude..=) Prelude.<$> s3
          ]
      )
