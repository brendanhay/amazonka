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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype NetworkInterfaceAttribute = NetworkInterfaceAttribute'
  { fromNetworkInterfaceAttribute ::
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
