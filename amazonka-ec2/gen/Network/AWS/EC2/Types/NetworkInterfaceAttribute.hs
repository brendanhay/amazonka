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
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttribute
  ( NetworkInterfaceAttribute
      ( ..,
        NetworkInterfaceAttribute_Attachment,
        NetworkInterfaceAttribute_Description,
        NetworkInterfaceAttribute_GroupSet,
        NetworkInterfaceAttribute_SourceDestCheck
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype NetworkInterfaceAttribute = NetworkInterfaceAttribute'
  { fromNetworkInterfaceAttribute ::
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

pattern NetworkInterfaceAttribute_Attachment :: NetworkInterfaceAttribute
pattern NetworkInterfaceAttribute_Attachment = NetworkInterfaceAttribute' "attachment"

pattern NetworkInterfaceAttribute_Description :: NetworkInterfaceAttribute
pattern NetworkInterfaceAttribute_Description = NetworkInterfaceAttribute' "description"

pattern NetworkInterfaceAttribute_GroupSet :: NetworkInterfaceAttribute
pattern NetworkInterfaceAttribute_GroupSet = NetworkInterfaceAttribute' "groupSet"

pattern NetworkInterfaceAttribute_SourceDestCheck :: NetworkInterfaceAttribute
pattern NetworkInterfaceAttribute_SourceDestCheck = NetworkInterfaceAttribute' "sourceDestCheck"

{-# COMPLETE
  NetworkInterfaceAttribute_Attachment,
  NetworkInterfaceAttribute_Description,
  NetworkInterfaceAttribute_GroupSet,
  NetworkInterfaceAttribute_SourceDestCheck,
  NetworkInterfaceAttribute'
  #-}
