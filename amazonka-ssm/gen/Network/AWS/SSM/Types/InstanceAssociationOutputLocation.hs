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
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.S3OutputLocation

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'newInstanceAssociationOutputLocation' smart constructor.
data InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { -- | An S3 bucket where you want to store the results of this request.
    s3Location :: Core.Maybe S3OutputLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceAssociationOutputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'instanceAssociationOutputLocation_s3Location' - An S3 bucket where you want to store the results of this request.
newInstanceAssociationOutputLocation ::
  InstanceAssociationOutputLocation
newInstanceAssociationOutputLocation =
  InstanceAssociationOutputLocation'
    { s3Location =
        Core.Nothing
    }

-- | An S3 bucket where you want to store the results of this request.
instanceAssociationOutputLocation_s3Location :: Lens.Lens' InstanceAssociationOutputLocation (Core.Maybe S3OutputLocation)
instanceAssociationOutputLocation_s3Location = Lens.lens (\InstanceAssociationOutputLocation' {s3Location} -> s3Location) (\s@InstanceAssociationOutputLocation' {} a -> s {s3Location = a} :: InstanceAssociationOutputLocation)

instance
  Core.FromJSON
    InstanceAssociationOutputLocation
  where
  parseJSON =
    Core.withObject
      "InstanceAssociationOutputLocation"
      ( \x ->
          InstanceAssociationOutputLocation'
            Core.<$> (x Core..:? "S3Location")
      )

instance
  Core.Hashable
    InstanceAssociationOutputLocation

instance
  Core.NFData
    InstanceAssociationOutputLocation

instance
  Core.ToJSON
    InstanceAssociationOutputLocation
  where
  toJSON InstanceAssociationOutputLocation' {..} =
    Core.object
      ( Core.catMaybes
          [("S3Location" Core..=) Core.<$> s3Location]
      )
