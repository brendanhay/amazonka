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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype RouteTableAssociationStateCode = RouteTableAssociationStateCode'
  { fromRouteTableAssociationStateCode ::
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
