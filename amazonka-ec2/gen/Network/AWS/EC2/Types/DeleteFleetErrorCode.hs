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
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetErrorCode
  ( DeleteFleetErrorCode
      ( ..,
        DeleteFleetErrorCode_FleetIdDoesNotExist,
        DeleteFleetErrorCode_FleetIdMalformed,
        DeleteFleetErrorCode_FleetNotInDeletableState,
        DeleteFleetErrorCode_UnexpectedError
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype DeleteFleetErrorCode = DeleteFleetErrorCode'
  { fromDeleteFleetErrorCode ::
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

pattern DeleteFleetErrorCode_FleetIdDoesNotExist :: DeleteFleetErrorCode
pattern DeleteFleetErrorCode_FleetIdDoesNotExist = DeleteFleetErrorCode' "fleetIdDoesNotExist"

pattern DeleteFleetErrorCode_FleetIdMalformed :: DeleteFleetErrorCode
pattern DeleteFleetErrorCode_FleetIdMalformed = DeleteFleetErrorCode' "fleetIdMalformed"

pattern DeleteFleetErrorCode_FleetNotInDeletableState :: DeleteFleetErrorCode
pattern DeleteFleetErrorCode_FleetNotInDeletableState = DeleteFleetErrorCode' "fleetNotInDeletableState"

pattern DeleteFleetErrorCode_UnexpectedError :: DeleteFleetErrorCode
pattern DeleteFleetErrorCode_UnexpectedError = DeleteFleetErrorCode' "unexpectedError"

{-# COMPLETE
  DeleteFleetErrorCode_FleetIdDoesNotExist,
  DeleteFleetErrorCode_FleetIdMalformed,
  DeleteFleetErrorCode_FleetNotInDeletableState,
  DeleteFleetErrorCode_UnexpectedError,
  DeleteFleetErrorCode'
  #-}
