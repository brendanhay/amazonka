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
-- Module      : Amazonka.EC2.Types.VpcCidrBlockStateCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcCidrBlockStateCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VpcCidrBlockStateCode = VpcCidrBlockStateCode'
  { fromVpcCidrBlockStateCode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
