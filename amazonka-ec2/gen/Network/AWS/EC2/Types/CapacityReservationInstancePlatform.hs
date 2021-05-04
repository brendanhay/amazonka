{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationInstancePlatform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationInstancePlatform
  ( CapacityReservationInstancePlatform
      ( ..,
        CapacityReservationInstancePlatform_Linux_UNIX,
        CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise,
        CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard,
        CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web,
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype CapacityReservationInstancePlatform = CapacityReservationInstancePlatform'
  { fromCapacityReservationInstancePlatform ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern CapacityReservationInstancePlatform_Linux_UNIX :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_UNIX = CapacityReservationInstancePlatform' "Linux/UNIX"

pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Enterprise = CapacityReservationInstancePlatform' "Linux with SQL Server Enterprise"

pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Standard = CapacityReservationInstancePlatform' "Linux with SQL Server Standard"

pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatform_Linux_with_SQL_Server_Web = CapacityReservationInstancePlatform' "Linux with SQL Server Web"

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
  CapacityReservationInstancePlatform_Red_Hat_Enterprise_Linux,
  CapacityReservationInstancePlatform_SUSE_Linux,
  CapacityReservationInstancePlatform_Windows,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server_Enterprise,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server_Standard,
  CapacityReservationInstancePlatform_Windows_with_SQL_Server_Web,
  CapacityReservationInstancePlatform'
  #-}
