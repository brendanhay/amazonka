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
-- Module      : Amazonka.EC2.Types.ArchitectureType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ArchitectureType
  ( ArchitectureType
      ( ..,
        ArchitectureType_Arm64,
        ArchitectureType_Arm64_mac,
        ArchitectureType_I386,
        ArchitectureType_X86_64,
        ArchitectureType_X86_64_mac
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ArchitectureType = ArchitectureType'
  { fromArchitectureType ::
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

pattern ArchitectureType_Arm64 :: ArchitectureType
pattern ArchitectureType_Arm64 = ArchitectureType' "arm64"

pattern ArchitectureType_Arm64_mac :: ArchitectureType
pattern ArchitectureType_Arm64_mac = ArchitectureType' "arm64_mac"

pattern ArchitectureType_I386 :: ArchitectureType
pattern ArchitectureType_I386 = ArchitectureType' "i386"

pattern ArchitectureType_X86_64 :: ArchitectureType
pattern ArchitectureType_X86_64 = ArchitectureType' "x86_64"

pattern ArchitectureType_X86_64_mac :: ArchitectureType
pattern ArchitectureType_X86_64_mac = ArchitectureType' "x86_64_mac"

{-# COMPLETE
  ArchitectureType_Arm64,
  ArchitectureType_Arm64_mac,
  ArchitectureType_I386,
  ArchitectureType_X86_64,
  ArchitectureType_X86_64_mac,
  ArchitectureType'
  #-}
