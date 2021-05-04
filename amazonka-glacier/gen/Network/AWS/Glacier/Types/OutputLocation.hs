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
-- Module      : Network.AWS.Glacier.Types.OutputLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.OutputLocation where

import Network.AWS.Glacier.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the location where the select job results are
-- stored.
--
-- /See:/ 'newOutputLocation' smart constructor.
data OutputLocation = OutputLocation'
  { -- | Describes an S3 location that will receive the results of the job
    -- request.
    s3 :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON OutputLocation where
  parseJSON =
    Prelude.withObject
      "OutputLocation"
      ( \x ->
          OutputLocation' Prelude.<$> (x Prelude..:? "S3")
      )

instance Prelude.Hashable OutputLocation

instance Prelude.NFData OutputLocation

instance Prelude.ToJSON OutputLocation where
  toJSON OutputLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("S3" Prelude..=) Prelude.<$> s3]
      )
