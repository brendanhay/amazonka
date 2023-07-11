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
-- Module      : Amazonka.EC2.Types.GatewayAssociationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.GatewayAssociationState
  ( GatewayAssociationState
      ( ..,
        GatewayAssociationState_Associated,
        GatewayAssociationState_Associating,
        GatewayAssociationState_Disassociating,
        GatewayAssociationState_Not_associated
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype GatewayAssociationState = GatewayAssociationState'
  { fromGatewayAssociationState ::
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

pattern GatewayAssociationState_Associated :: GatewayAssociationState
pattern GatewayAssociationState_Associated = GatewayAssociationState' "associated"

pattern GatewayAssociationState_Associating :: GatewayAssociationState
pattern GatewayAssociationState_Associating = GatewayAssociationState' "associating"

pattern GatewayAssociationState_Disassociating :: GatewayAssociationState
pattern GatewayAssociationState_Disassociating = GatewayAssociationState' "disassociating"

pattern GatewayAssociationState_Not_associated :: GatewayAssociationState
pattern GatewayAssociationState_Not_associated = GatewayAssociationState' "not-associated"

{-# COMPLETE
  GatewayAssociationState_Associated,
  GatewayAssociationState_Associating,
  GatewayAssociationState_Disassociating,
  GatewayAssociationState_Not_associated,
  GatewayAssociationState'
  #-}
