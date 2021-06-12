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
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockStateCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockStateCode
  ( SubnetCidrBlockStateCode
      ( ..,
        SubnetCidrBlockStateCode_Associated,
        SubnetCidrBlockStateCode_Associating,
        SubnetCidrBlockStateCode_Disassociated,
        SubnetCidrBlockStateCode_Disassociating,
        SubnetCidrBlockStateCode_Failed,
        SubnetCidrBlockStateCode_Failing
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype SubnetCidrBlockStateCode = SubnetCidrBlockStateCode'
  { fromSubnetCidrBlockStateCode ::
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

pattern SubnetCidrBlockStateCode_Associated :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCode_Associated = SubnetCidrBlockStateCode' "associated"

pattern SubnetCidrBlockStateCode_Associating :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCode_Associating = SubnetCidrBlockStateCode' "associating"

pattern SubnetCidrBlockStateCode_Disassociated :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCode_Disassociated = SubnetCidrBlockStateCode' "disassociated"

pattern SubnetCidrBlockStateCode_Disassociating :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCode_Disassociating = SubnetCidrBlockStateCode' "disassociating"

pattern SubnetCidrBlockStateCode_Failed :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCode_Failed = SubnetCidrBlockStateCode' "failed"

pattern SubnetCidrBlockStateCode_Failing :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCode_Failing = SubnetCidrBlockStateCode' "failing"

{-# COMPLETE
  SubnetCidrBlockStateCode_Associated,
  SubnetCidrBlockStateCode_Associating,
  SubnetCidrBlockStateCode_Disassociated,
  SubnetCidrBlockStateCode_Disassociating,
  SubnetCidrBlockStateCode_Failed,
  SubnetCidrBlockStateCode_Failing,
  SubnetCidrBlockStateCode'
  #-}
