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

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.S3Location
import qualified Network.AWS.Lens as Lens

-- | Contains information about the location where the select job results are
-- stored.
--
-- /See:/ 'newOutputLocation' smart constructor.
data OutputLocation = OutputLocation'
  { -- | Describes an S3 location that will receive the results of the job
    -- request.
    s3 :: Core.Maybe S3Location
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  OutputLocation' {s3 = Core.Nothing}

-- | Describes an S3 location that will receive the results of the job
-- request.
outputLocation_s3 :: Lens.Lens' OutputLocation (Core.Maybe S3Location)
outputLocation_s3 = Lens.lens (\OutputLocation' {s3} -> s3) (\s@OutputLocation' {} a -> s {s3 = a} :: OutputLocation)

instance Core.FromJSON OutputLocation where
  parseJSON =
    Core.withObject
      "OutputLocation"
      (\x -> OutputLocation' Core.<$> (x Core..:? "S3"))

instance Core.Hashable OutputLocation

instance Core.NFData OutputLocation

instance Core.ToJSON OutputLocation where
  toJSON OutputLocation' {..} =
    Core.object
      (Core.catMaybes [("S3" Core..=) Core.<$> s3])
