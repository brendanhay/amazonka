{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.CapacityReservationInstancePlatform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationInstancePlatform
  ( CapacityReservationInstancePlatform
      ( ..,
        CapacityReservationInstancePlatform_Linux_UNIX,
        CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise,
        CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard,
        CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web,
        CapacityReservationInstancePlatform_RHEL_with_HA,
        CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Enterprise,
        CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Standard,
        CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Enterprise,
        CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Standard,
        CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Web,
        CapacityReservationInstancePlatform_Red_Hat_Enterprise_Linux,
        CapacityReservationInstancePlatform_SUSE_Linux,
        CapacityReservationInstancePlatform_Windows,
        CapacityReservationInstancePlatform_Windows_with_SQL_Server,
        CapacityReservationInstancePlatform_Windows_with_SQL_Server_Enterprise,
        CapacityReservationInstancePlatform_Windows_with_SQL_Server_Standard,
        CapacityReservationInstancePlatform_Windows_with_SQL_Server_Web
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype CapacityReservationInstancePlatform = CapacityReservationInstancePlatform'
  { fromCapacityReservationInstancePlatform ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern CapacityReservationInstancePlatform_Linux_UNIX :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_UNIX = CapacityReservationInstancePlatform' "Linux/UNIX"

pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise = CapacityReservationInstancePlatform' "Linux with SQL Server Enterprise"

pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard = CapacityReservationInstancePlatform' "Linux with SQL Server Standard"

pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web = CapacityReservationInstancePlatform' "Linux with SQL Server Web"

pattern CapacityReservationInstancePlatform_RHEL_with_HA :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_RHEL_with_HA = CapacityReservationInstancePlatform' "RHEL with HA"

pattern CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Enterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Enterprise = CapacityReservationInstancePlatform' "RHEL with HA and SQL Server Enterprise"

pattern CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Standard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Standard = CapacityReservationInstancePlatform' "RHEL with HA and SQL Server Standard"

pattern CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Enterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Enterprise = CapacityReservationInstancePlatform' "RHEL with SQL Server Enterprise"

pattern CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Standard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Standard = CapacityReservationInstancePlatform' "RHEL with SQL Server Standard"

pattern CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Web :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Web = CapacityReservationInstancePlatform' "RHEL with SQL Server Web"

pattern CapacityReservationInstancePlatform_Red_Hat_Enterprise_Linux :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Red_Hat_Enterprise_Linux = CapacityReservationInstancePlatform' "Red Hat Enterprise Linux"

pattern CapacityReservationInstancePlatform_SUSE_Linux :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_SUSE_Linux = CapacityReservationInstancePlatform' "SUSE Linux"

pattern CapacityReservationInstancePlatform_Windows :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Windows = CapacityReservationInstancePlatform' "Windows"

pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server = CapacityReservationInstancePlatform' "Windows with SQL Server"

pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server_Enterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server_Enterprise = CapacityReservationInstancePlatform' "Windows with SQL Server Enterprise"

pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server_Standard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server_Standard = CapacityReservationInstancePlatform' "Windows with SQL Server Standard"

pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server_Web :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Windows_with_SQL_Server_Web = CapacityReservationInstancePlatform' "Windows with SQL Server Web"

{-# COMPLETE
  CapacityReservationInstancePlatform_Linux_UNIX,
  CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise,
  CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard,
  CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web,
  CapacityReservationInstancePlatform_RHEL_with_HA,
  CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Enterprise,
  CapacityReservationInstancePlatform_RHEL_with_HA_and_SQL_Server_Standard,
  CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Enterprise,
  CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Standard,
  CapacityReservationInstancePlatform_RHEL_with_SQL_Server_Web,
  CapacityReservationInstancePlatform_Red_Hat_Enterprise_Linux,
  CapacityReservationInstancePlatform_SUSE_Linux,
  CapacityReservationInstancePlatform_Windows,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server_Enterprise,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server_Standard,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server_Web,
  CapacityReservationInstancePlatform'
  #-}
