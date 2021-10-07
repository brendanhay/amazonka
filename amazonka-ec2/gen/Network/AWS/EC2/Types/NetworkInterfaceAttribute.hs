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
import qualified Network.AWS.Prelude as Prelude

newtype NetworkInterfaceAttribute = NetworkInterfaceAttribute'
  { fromNetworkInterfaceAttribute ::
      Core.Text
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
