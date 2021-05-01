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
-- Module      : Network.AWS.Redshift.Types.AvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AvailabilityZone where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SupportedPlatform

-- | Describes an availability zone.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the availability zone.
    name :: Prelude.Maybe Prelude.Text,
    supportedPlatforms :: Prelude.Maybe [SupportedPlatform]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'availabilityZone_name' - The name of the availability zone.
--
-- 'supportedPlatforms', 'availabilityZone_supportedPlatforms' -
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { name = Prelude.Nothing,
      supportedPlatforms = Prelude.Nothing
    }

-- | The name of the availability zone.
availabilityZone_name :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_name = Lens.lens (\AvailabilityZone' {name} -> name) (\s@AvailabilityZone' {} a -> s {name = a} :: AvailabilityZone)

-- |
availabilityZone_supportedPlatforms :: Lens.Lens' AvailabilityZone (Prelude.Maybe [SupportedPlatform])
availabilityZone_supportedPlatforms = Lens.lens (\AvailabilityZone' {supportedPlatforms} -> supportedPlatforms) (\s@AvailabilityZone' {} a -> s {supportedPlatforms = a} :: AvailabilityZone) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Prelude.<$> (x Prelude..@? "Name")
      Prelude.<*> ( x Prelude..@? "SupportedPlatforms"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "SupportedPlatform")
                  )

instance Prelude.Hashable AvailabilityZone

instance Prelude.NFData AvailabilityZone
