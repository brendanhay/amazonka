{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RIProductDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RIProductDescription
  ( RIProductDescription
      ( RIProductDescription',
        LinuxUnix,
        LinuxUnixAmazonVPC,
        Windows,
        WindowsAmazonVPC
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RIProductDescription = RIProductDescription' Lude.Text
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

pattern LinuxUnix :: RIProductDescription
pattern LinuxUnix = RIProductDescription' "Linux/UNIX"

pattern LinuxUnixAmazonVPC :: RIProductDescription
pattern LinuxUnixAmazonVPC = RIProductDescription' "Linux/UNIX (Amazon VPC)"

pattern Windows :: RIProductDescription
pattern Windows = RIProductDescription' "Windows"

pattern WindowsAmazonVPC :: RIProductDescription
pattern WindowsAmazonVPC = RIProductDescription' "Windows (Amazon VPC)"

{-# COMPLETE
  LinuxUnix,
  LinuxUnixAmazonVPC,
  Windows,
  WindowsAmazonVPC,
  RIProductDescription'
  #-}
