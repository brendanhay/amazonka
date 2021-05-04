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
-- Module      : Network.AWS.APIGateway.Types.VpcLinkStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.VpcLinkStatus
  ( VpcLinkStatus
      ( ..,
        VpcLinkStatus_AVAILABLE,
        VpcLinkStatus_DELETING,
        VpcLinkStatus_FAILED,
        VpcLinkStatus_PENDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype VpcLinkStatus = VpcLinkStatus'
  { fromVpcLinkStatus ::
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

pattern VpcLinkStatus_AVAILABLE :: VpcLinkStatus
pattern VpcLinkStatus_AVAILABLE = VpcLinkStatus' "AVAILABLE"

pattern VpcLinkStatus_DELETING :: VpcLinkStatus
pattern VpcLinkStatus_DELETING = VpcLinkStatus' "DELETING"

pattern VpcLinkStatus_FAILED :: VpcLinkStatus
pattern VpcLinkStatus_FAILED = VpcLinkStatus' "FAILED"

pattern VpcLinkStatus_PENDING :: VpcLinkStatus
pattern VpcLinkStatus_PENDING = VpcLinkStatus' "PENDING"

{-# COMPLETE
  VpcLinkStatus_AVAILABLE,
  VpcLinkStatus_DELETING,
  VpcLinkStatus_FAILED,
  VpcLinkStatus_PENDING,
  VpcLinkStatus'
  #-}
