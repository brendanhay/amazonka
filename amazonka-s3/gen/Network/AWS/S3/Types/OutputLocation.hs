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
-- Module      : Network.AWS.S3.Types.OutputLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OutputLocation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.S3Location

-- | Describes the location where the restore job\'s output is stored.
--
-- /See:/ 'newOutputLocation' smart constructor.
data OutputLocation = OutputLocation'
  { -- | Describes an S3 location that will receive the results of the restore
    -- request.
    s3 :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'outputLocation_s3' - Describes an S3 location that will receive the results of the restore
-- request.
newOutputLocation ::
  OutputLocation
newOutputLocation =
  OutputLocation' {s3 = Prelude.Nothing}

-- | Describes an S3 location that will receive the results of the restore
-- request.
outputLocation_s3 :: Lens.Lens' OutputLocation (Prelude.Maybe S3Location)
outputLocation_s3 = Lens.lens (\OutputLocation' {s3} -> s3) (\s@OutputLocation' {} a -> s {s3 = a} :: OutputLocation)

instance Prelude.Hashable OutputLocation

instance Prelude.NFData OutputLocation

instance Prelude.ToXML OutputLocation where
  toXML OutputLocation' {..} =
    Prelude.mconcat ["S3" Prelude.@= s3]
