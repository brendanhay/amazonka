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
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockStateCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcCidrBlockStateCode
  ( VpcCidrBlockStateCode
      ( ..,
        VpcCidrBlockStateCode_Associated,
        VpcCidrBlockStateCode_Associating,
        VpcCidrBlockStateCode_Disassociated,
        VpcCidrBlockStateCode_Disassociating,
        VpcCidrBlockStateCode_Failed,
        VpcCidrBlockStateCode_Failing
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VpcCidrBlockStateCode = VpcCidrBlockStateCode'
  { fromVpcCidrBlockStateCode ::
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

pattern VpcCidrBlockStateCode_Associated :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCode_Associated = VpcCidrBlockStateCode' "associated"

pattern VpcCidrBlockStateCode_Associating :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCode_Associating = VpcCidrBlockStateCode' "associating"

pattern VpcCidrBlockStateCode_Disassociated :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCode_Disassociated = VpcCidrBlockStateCode' "disassociated"

pattern VpcCidrBlockStateCode_Disassociating :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCode_Disassociating = VpcCidrBlockStateCode' "disassociating"

pattern VpcCidrBlockStateCode_Failed :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCode_Failed = VpcCidrBlockStateCode' "failed"

pattern VpcCidrBlockStateCode_Failing :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCode_Failing = VpcCidrBlockStateCode' "failing"

{-# COMPLETE
  VpcCidrBlockStateCode_Associated,
  VpcCidrBlockStateCode_Associating,
  VpcCidrBlockStateCode_Disassociated,
  VpcCidrBlockStateCode_Disassociating,
  VpcCidrBlockStateCode_Failed,
  VpcCidrBlockStateCode_Failing,
  VpcCidrBlockStateCode'
  #-}
