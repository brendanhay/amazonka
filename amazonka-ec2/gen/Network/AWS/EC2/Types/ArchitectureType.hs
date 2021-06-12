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
-- Module      : Network.AWS.EC2.Types.ArchitectureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ArchitectureType
  ( ArchitectureType
      ( ..,
        ArchitectureType_Arm64,
        ArchitectureType_I386,
        ArchitectureType_X86_64
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ArchitectureType = ArchitectureType'
  { fromArchitectureType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ArchitectureType_Arm64 :: ArchitectureType
pattern ArchitectureType_Arm64 = ArchitectureType' "arm64"

pattern ArchitectureType_I386 :: ArchitectureType
pattern ArchitectureType_I386 = ArchitectureType' "i386"

pattern ArchitectureType_X86_64 :: ArchitectureType
pattern ArchitectureType_X86_64 = ArchitectureType' "x86_64"

{-# COMPLETE
  ArchitectureType_Arm64,
  ArchitectureType_I386,
  ArchitectureType_X86_64,
  ArchitectureType'
  #-}
