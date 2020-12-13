{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ArchitectureValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ArchitectureValues
  ( ArchitectureValues
      ( ArchitectureValues',
        I386,
        X86_64,
        ARM64
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ArchitectureValues = ArchitectureValues' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern I386 :: ArchitectureValues
pattern I386 = ArchitectureValues' "i386"

pattern X86_64 :: ArchitectureValues
pattern X86_64 = ArchitectureValues' "x86_64"

pattern ARM64 :: ArchitectureValues
pattern ARM64 = ArchitectureValues' "arm64"

{-# COMPLETE
  I386,
  X86_64,
  ARM64,
  ArchitectureValues'
  #-}
