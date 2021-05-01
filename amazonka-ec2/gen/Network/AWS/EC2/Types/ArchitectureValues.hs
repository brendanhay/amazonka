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
-- Module      : Network.AWS.EC2.Types.ArchitectureValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ArchitectureValues
  ( ArchitectureValues
      ( ..,
        ArchitectureValues_Arm64,
        ArchitectureValues_I386,
        ArchitectureValues_X86_64
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ArchitectureValues = ArchitectureValues'
  { fromArchitectureValues ::
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

pattern ArchitectureValues_Arm64 :: ArchitectureValues
pattern ArchitectureValues_Arm64 = ArchitectureValues' "arm64"

pattern ArchitectureValues_I386 :: ArchitectureValues
pattern ArchitectureValues_I386 = ArchitectureValues' "i386"

pattern ArchitectureValues_X86_64 :: ArchitectureValues
pattern ArchitectureValues_X86_64 = ArchitectureValues' "x86_64"

{-# COMPLETE
  ArchitectureValues_Arm64,
  ArchitectureValues_I386,
  ArchitectureValues_X86_64,
  ArchitectureValues'
  #-}
