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
-- Module      : Amazonka.EC2.Types.RouteTableAssociationStateCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RouteTableAssociationStateCode
  ( RouteTableAssociationStateCode
      ( ..,
        RouteTableAssociationStateCode_Associated,
        RouteTableAssociationStateCode_Associating,
        RouteTableAssociationStateCode_Disassociated,
        RouteTableAssociationStateCode_Disassociating,
        RouteTableAssociationStateCode_Failed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype RouteTableAssociationStateCode = RouteTableAssociationStateCode'
  { fromRouteTableAssociationStateCode ::
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

pattern RouteTableAssociationStateCode_Associated :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCode_Associated = RouteTableAssociationStateCode' "associated"

pattern RouteTableAssociationStateCode_Associating :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCode_Associating = RouteTableAssociationStateCode' "associating"

pattern RouteTableAssociationStateCode_Disassociated :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCode_Disassociated = RouteTableAssociationStateCode' "disassociated"

pattern RouteTableAssociationStateCode_Disassociating :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCode_Disassociating = RouteTableAssociationStateCode' "disassociating"

pattern RouteTableAssociationStateCode_Failed :: RouteTableAssociationStateCode
pattern RouteTableAssociationStateCode_Failed = RouteTableAssociationStateCode' "failed"

{-# COMPLETE
  RouteTableAssociationStateCode_Associated,
  RouteTableAssociationStateCode_Associating,
  RouteTableAssociationStateCode_Disassociated,
  RouteTableAssociationStateCode_Disassociating,
  RouteTableAssociationStateCode_Failed,
  RouteTableAssociationStateCode'
  #-}
