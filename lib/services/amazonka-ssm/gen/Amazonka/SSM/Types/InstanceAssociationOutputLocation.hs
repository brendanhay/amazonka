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
-- Module      : Amazonka.SSM.Types.InstanceAssociationOutputLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceAssociationOutputLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.S3OutputLocation

-- | An S3 bucket where you want to store the results of this request.
--
-- For the minimal permissions required to enable Amazon S3 output for an
-- association, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-state-assoc.html Creating associations>
-- in the /Systems Manager User Guide/.
--
-- /See:/ 'newInstanceAssociationOutputLocation' smart constructor.
data InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { -- | An S3 bucket where you want to store the results of this request.
    s3Location :: Prelude.Maybe S3OutputLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromJSON
    InstanceAssociationOutputLocation
  where
  parseJSON =
    Data.withObject
      "InstanceAssociationOutputLocation"
      ( \x ->
          InstanceAssociationOutputLocation'
            Prelude.<$> (x Data..:? "S3Location")
      )

instance
  Prelude.Hashable
    InstanceAssociationOutputLocation
  where
  hashWithSalt
    _salt
    InstanceAssociationOutputLocation' {..} =
      _salt `Prelude.hashWithSalt` s3Location

instance
  Prelude.NFData
    InstanceAssociationOutputLocation
  where
  rnf InstanceAssociationOutputLocation' {..} =
    Prelude.rnf s3Location

instance
  Data.ToJSON
    InstanceAssociationOutputLocation
  where
  toJSON InstanceAssociationOutputLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Location" Data..=) Prelude.<$> s3Location]
      )
