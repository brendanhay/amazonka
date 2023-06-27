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
-- Module      : Amazonka.Glue.Types.Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CodeGenNodeArg
import qualified Amazonka.Prelude as Prelude

-- | The location of resources.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | An Amazon DynamoDB table location.
    dynamoDB :: Prelude.Maybe [CodeGenNodeArg],
    -- | A JDBC location.
    jdbc :: Prelude.Maybe [CodeGenNodeArg],
    -- | An Amazon Simple Storage Service (Amazon S3) location.
    s3 :: Prelude.Maybe [CodeGenNodeArg]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamoDB', 'location_dynamoDB' - An Amazon DynamoDB table location.
--
-- 'jdbc', 'location_jdbc' - A JDBC location.
--
-- 's3', 'location_s3' - An Amazon Simple Storage Service (Amazon S3) location.
newLocation ::
  Location
newLocation =
  Location'
    { dynamoDB = Prelude.Nothing,
      jdbc = Prelude.Nothing,
      s3 = Prelude.Nothing
    }

-- | An Amazon DynamoDB table location.
location_dynamoDB :: Lens.Lens' Location (Prelude.Maybe [CodeGenNodeArg])
location_dynamoDB = Lens.lens (\Location' {dynamoDB} -> dynamoDB) (\s@Location' {} a -> s {dynamoDB = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | A JDBC location.
location_jdbc :: Lens.Lens' Location (Prelude.Maybe [CodeGenNodeArg])
location_jdbc = Lens.lens (\Location' {jdbc} -> jdbc) (\s@Location' {} a -> s {jdbc = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon Simple Storage Service (Amazon S3) location.
location_s3 :: Lens.Lens' Location (Prelude.Maybe [CodeGenNodeArg])
location_s3 = Lens.lens (\Location' {s3} -> s3) (\s@Location' {} a -> s {s3 = a} :: Location) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Location where
  hashWithSalt _salt Location' {..} =
    _salt
      `Prelude.hashWithSalt` dynamoDB
      `Prelude.hashWithSalt` jdbc
      `Prelude.hashWithSalt` s3

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf dynamoDB
      `Prelude.seq` Prelude.rnf jdbc
      `Prelude.seq` Prelude.rnf s3

instance Data.ToJSON Location where
  toJSON Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DynamoDB" Data..=) Prelude.<$> dynamoDB,
            ("Jdbc" Data..=) Prelude.<$> jdbc,
            ("S3" Data..=) Prelude.<$> s3
          ]
      )
