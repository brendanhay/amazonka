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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype SubnetCidrBlockStateCode = SubnetCidrBlockStateCode'
  { fromSubnetCidrBlockStateCode ::
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
