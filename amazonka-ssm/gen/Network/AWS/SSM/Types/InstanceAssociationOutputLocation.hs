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
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.S3OutputLocation

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'newInstanceAssociationOutputLocation' smart constructor.
data InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { -- | An S3 bucket where you want to store the results of this request.
    s3Location :: Prelude.Maybe S3OutputLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | An S3 bucket where you want to store the results of this request.
instanceAssociationOutputLocation_s3Location :: Lens.Lens' InstanceAssociationOutputLocation (Prelude.Maybe S3OutputLocation)
instanceAssociationOutputLocation_s3Location = Lens.lens (\InstanceAssociationOutputLocation' {s3Location} -> s3Location) (\s@InstanceAssociationOutputLocation' {} a -> s {s3Location = a} :: InstanceAssociationOutputLocation)

instance
  Prelude.FromJSON
    InstanceAssociationOutputLocation
  where
  parseJSON =
    Prelude.withObject
      "InstanceAssociationOutputLocation"
      ( \x ->
          InstanceAssociationOutputLocation'
            Prelude.<$> (x Prelude..:? "S3Location")
      )

instance
  Prelude.Hashable
    InstanceAssociationOutputLocation

instance
  Prelude.NFData
    InstanceAssociationOutputLocation

instance
  Prelude.ToJSON
    InstanceAssociationOutputLocation
  where
  toJSON InstanceAssociationOutputLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("S3Location" Prelude..=) Prelude.<$> s3Location]
      )
