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
-- Module      : Network.AWS.Kendra.Types.FaqStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.FaqStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FaqStatus = FaqStatus'
  { fromFaqStatus ::
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
