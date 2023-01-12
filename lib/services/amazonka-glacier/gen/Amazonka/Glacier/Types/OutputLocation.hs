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
-- Module      : Amazonka.Glacier.Types.OutputLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.OutputLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the location where the select job results are
-- stored.
--
-- /See:/ 'newOutputLocation' smart constructor.
data OutputLocation = OutputLocation'
  { -- | Describes an S3 location that will receive the results of the job
    -- request.
    s3 :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'outputLocation_s3' - Describes an S3 location that will receive the results of the job
-- request.
newOutputLocation ::
  OutputLocation
newOutputLocation =
  OutputLocation' {s3 = Prelude.Nothing}

-- | Describes an S3 location that will receive the results of the job
-- request.
outputLocation_s3 :: Lens.Lens' OutputLocation (Prelude.Maybe S3Location)
outputLocation_s3 = Lens.lens (\OutputLocation' {s3} -> s3) (\s@OutputLocation' {} a -> s {s3 = a} :: OutputLocation)

instance Data.FromJSON OutputLocation where
  parseJSON =
    Data.withObject
      "OutputLocation"
      ( \x ->
          OutputLocation' Prelude.<$> (x Data..:? "S3")
      )

instance Prelude.Hashable OutputLocation where
  hashWithSalt _salt OutputLocation' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData OutputLocation where
  rnf OutputLocation' {..} = Prelude.rnf s3

instance Data.ToJSON OutputLocation where
  toJSON OutputLocation' {..} =
    Data.object
      (Prelude.catMaybes [("S3" Data..=) Prelude.<$> s3])
