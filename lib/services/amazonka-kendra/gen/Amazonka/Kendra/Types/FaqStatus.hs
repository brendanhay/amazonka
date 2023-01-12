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
-- Module      : Amazonka.Kendra.Types.FaqStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FaqStatus
  ( FaqStatus
      ( ..,
        FaqStatus_ACTIVE,
        FaqStatus_CREATING,
        FaqStatus_DELETING,
        FaqStatus_FAILED,
        FaqStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FaqStatus = FaqStatus'
  { fromFaqStatus ::
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

pattern FaqStatus_ACTIVE :: FaqStatus
pattern FaqStatus_ACTIVE = FaqStatus' "ACTIVE"

pattern FaqStatus_CREATING :: FaqStatus
pattern FaqStatus_CREATING = FaqStatus' "CREATING"

pattern FaqStatus_DELETING :: FaqStatus
pattern FaqStatus_DELETING = FaqStatus' "DELETING"

pattern FaqStatus_FAILED :: FaqStatus
pattern FaqStatus_FAILED = FaqStatus' "FAILED"

pattern FaqStatus_UPDATING :: FaqStatus
pattern FaqStatus_UPDATING = FaqStatus' "UPDATING"

{-# COMPLETE
  FaqStatus_ACTIVE,
  FaqStatus_CREATING,
  FaqStatus_DELETING,
  FaqStatus_FAILED,
  FaqStatus_UPDATING,
  FaqStatus'
  #-}
