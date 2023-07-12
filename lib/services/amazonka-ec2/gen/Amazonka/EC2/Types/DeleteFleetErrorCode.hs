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
-- Module      : Amazonka.EC2.Types.DeleteFleetErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeleteFleetErrorCode
  ( DeleteFleetErrorCode
      ( ..,
        DeleteFleetErrorCode_FleetIdDoesNotExist,
        DeleteFleetErrorCode_FleetIdMalformed,
        DeleteFleetErrorCode_FleetNotInDeletableState,
        DeleteFleetErrorCode_UnexpectedError
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype DeleteFleetErrorCode = DeleteFleetErrorCode'
  { fromDeleteFleetErrorCode ::
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
