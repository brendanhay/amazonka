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
-- Module      : Amazonka.CloudFormation.Types.RegistrationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.RegistrationStatus
  ( RegistrationStatus
      ( ..,
        RegistrationStatus_COMPLETE,
        RegistrationStatus_FAILED,
        RegistrationStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RegistrationStatus = RegistrationStatus'
  { fromRegistrationStatus ::
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

pattern RegistrationStatus_COMPLETE :: RegistrationStatus
pattern RegistrationStatus_COMPLETE = RegistrationStatus' "COMPLETE"

pattern RegistrationStatus_FAILED :: RegistrationStatus
pattern RegistrationStatus_FAILED = RegistrationStatus' "FAILED"

pattern RegistrationStatus_IN_PROGRESS :: RegistrationStatus
pattern RegistrationStatus_IN_PROGRESS = RegistrationStatus' "IN_PROGRESS"

{-# COMPLETE
  RegistrationStatus_COMPLETE,
  RegistrationStatus_FAILED,
  RegistrationStatus_IN_PROGRESS,
  RegistrationStatus'
  #-}
