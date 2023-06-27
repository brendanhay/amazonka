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
-- Module      : Amazonka.SSM.Types.OperatingSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OperatingSystem
  ( OperatingSystem
      ( ..,
        OperatingSystem_ALMA_LINUX,
        OperatingSystem_AMAZON_LINUX,
        OperatingSystem_AMAZON_LINUX_2,
        OperatingSystem_AMAZON_LINUX_2022,
        OperatingSystem_AMAZON_LINUX_2023,
        OperatingSystem_CENTOS,
        OperatingSystem_DEBIAN,
        OperatingSystem_MACOS,
        OperatingSystem_ORACLE_LINUX,
        OperatingSystem_RASPBIAN,
        OperatingSystem_REDHAT_ENTERPRISE_LINUX,
        OperatingSystem_ROCKY_LINUX,
        OperatingSystem_SUSE,
        OperatingSystem_UBUNTU,
        OperatingSystem_WINDOWS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperatingSystem = OperatingSystem'
  { fromOperatingSystem ::
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

pattern OperatingSystem_ALMA_LINUX :: OperatingSystem
pattern OperatingSystem_ALMA_LINUX = OperatingSystem' "ALMA_LINUX"

pattern OperatingSystem_AMAZON_LINUX :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX = OperatingSystem' "AMAZON_LINUX"

pattern OperatingSystem_AMAZON_LINUX_2 :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX_2 = OperatingSystem' "AMAZON_LINUX_2"

pattern OperatingSystem_AMAZON_LINUX_2022 :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX_2022 = OperatingSystem' "AMAZON_LINUX_2022"

pattern OperatingSystem_AMAZON_LINUX_2023 :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX_2023 = OperatingSystem' "AMAZON_LINUX_2023"

pattern OperatingSystem_CENTOS :: OperatingSystem
pattern OperatingSystem_CENTOS = OperatingSystem' "CENTOS"

pattern OperatingSystem_DEBIAN :: OperatingSystem
pattern OperatingSystem_DEBIAN = OperatingSystem' "DEBIAN"

pattern OperatingSystem_MACOS :: OperatingSystem
pattern OperatingSystem_MACOS = OperatingSystem' "MACOS"

pattern OperatingSystem_ORACLE_LINUX :: OperatingSystem
pattern OperatingSystem_ORACLE_LINUX = OperatingSystem' "ORACLE_LINUX"

pattern OperatingSystem_RASPBIAN :: OperatingSystem
pattern OperatingSystem_RASPBIAN = OperatingSystem' "RASPBIAN"

pattern OperatingSystem_REDHAT_ENTERPRISE_LINUX :: OperatingSystem
pattern OperatingSystem_REDHAT_ENTERPRISE_LINUX = OperatingSystem' "REDHAT_ENTERPRISE_LINUX"

pattern OperatingSystem_ROCKY_LINUX :: OperatingSystem
pattern OperatingSystem_ROCKY_LINUX = OperatingSystem' "ROCKY_LINUX"

pattern OperatingSystem_SUSE :: OperatingSystem
pattern OperatingSystem_SUSE = OperatingSystem' "SUSE"

pattern OperatingSystem_UBUNTU :: OperatingSystem
pattern OperatingSystem_UBUNTU = OperatingSystem' "UBUNTU"

pattern OperatingSystem_WINDOWS :: OperatingSystem
pattern OperatingSystem_WINDOWS = OperatingSystem' "WINDOWS"

{-# COMPLETE
  OperatingSystem_ALMA_LINUX,
  OperatingSystem_AMAZON_LINUX,
  OperatingSystem_AMAZON_LINUX_2,
  OperatingSystem_AMAZON_LINUX_2022,
  OperatingSystem_AMAZON_LINUX_2023,
  OperatingSystem_CENTOS,
  OperatingSystem_DEBIAN,
  OperatingSystem_MACOS,
  OperatingSystem_ORACLE_LINUX,
  OperatingSystem_RASPBIAN,
  OperatingSystem_REDHAT_ENTERPRISE_LINUX,
  OperatingSystem_ROCKY_LINUX,
  OperatingSystem_SUSE,
  OperatingSystem_UBUNTU,
  OperatingSystem_WINDOWS,
  OperatingSystem'
  #-}
