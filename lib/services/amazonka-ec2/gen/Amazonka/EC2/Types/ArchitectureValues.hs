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
-- Module      : Amazonka.EC2.Types.ArchitectureValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ArchitectureValues
  ( ArchitectureValues
      ( ..,
        ArchitectureValues_Arm64,
        ArchitectureValues_Arm64_mac,
        ArchitectureValues_I386,
        ArchitectureValues_X86_64,
        ArchitectureValues_X86_64_mac
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ArchitectureValues = ArchitectureValues'
  { fromArchitectureValues ::
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

pattern ArchitectureValues_Arm64 :: ArchitectureValues
pattern ArchitectureValues_Arm64 = ArchitectureValues' "arm64"

pattern ArchitectureValues_Arm64_mac :: ArchitectureValues
pattern ArchitectureValues_Arm64_mac = ArchitectureValues' "arm64_mac"

pattern ArchitectureValues_I386 :: ArchitectureValues
pattern ArchitectureValues_I386 = ArchitectureValues' "i386"

pattern ArchitectureValues_X86_64 :: ArchitectureValues
pattern ArchitectureValues_X86_64 = ArchitectureValues' "x86_64"

pattern ArchitectureValues_X86_64_mac :: ArchitectureValues
pattern ArchitectureValues_X86_64_mac = ArchitectureValues' "x86_64_mac"

{-# COMPLETE
  ArchitectureValues_Arm64,
  ArchitectureValues_Arm64_mac,
  ArchitectureValues_I386,
  ArchitectureValues_X86_64,
  ArchitectureValues_X86_64_mac,
  ArchitectureValues'
  #-}
