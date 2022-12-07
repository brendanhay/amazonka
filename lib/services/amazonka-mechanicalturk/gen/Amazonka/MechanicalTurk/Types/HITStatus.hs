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
-- Module      : Amazonka.MechanicalTurk.Types.HITStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.HITStatus
  ( HITStatus
      ( ..,
        HITStatus_Assignable,
        HITStatus_Disposed,
        HITStatus_Reviewable,
        HITStatus_Reviewing,
        HITStatus_Unassignable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HITStatus = HITStatus'
  { fromHITStatus ::
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

pattern HITStatus_Assignable :: HITStatus
pattern HITStatus_Assignable = HITStatus' "Assignable"

pattern HITStatus_Disposed :: HITStatus
pattern HITStatus_Disposed = HITStatus' "Disposed"

pattern HITStatus_Reviewable :: HITStatus
pattern HITStatus_Reviewable = HITStatus' "Reviewable"

pattern HITStatus_Reviewing :: HITStatus
pattern HITStatus_Reviewing = HITStatus' "Reviewing"

pattern HITStatus_Unassignable :: HITStatus
pattern HITStatus_Unassignable = HITStatus' "Unassignable"

{-# COMPLETE
  HITStatus_Assignable,
  HITStatus_Disposed,
  HITStatus_Reviewable,
  HITStatus_Reviewing,
  HITStatus_Unassignable,
  HITStatus'
  #-}
