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
-- Module      : Amazonka.ResilienceHub.Types.ResourceResolutionStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResourceResolutionStatusType
  ( ResourceResolutionStatusType
      ( ..,
        ResourceResolutionStatusType_Failed,
        ResourceResolutionStatusType_InProgress,
        ResourceResolutionStatusType_Pending,
        ResourceResolutionStatusType_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ResourceResolutionStatusType = ResourceResolutionStatusType'
  { fromResourceResolutionStatusType ::
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

pattern ResourceResolutionStatusType_Failed :: ResourceResolutionStatusType
pattern ResourceResolutionStatusType_Failed = ResourceResolutionStatusType' "Failed"

pattern ResourceResolutionStatusType_InProgress :: ResourceResolutionStatusType
pattern ResourceResolutionStatusType_InProgress = ResourceResolutionStatusType' "InProgress"

pattern ResourceResolutionStatusType_Pending :: ResourceResolutionStatusType
pattern ResourceResolutionStatusType_Pending = ResourceResolutionStatusType' "Pending"

pattern ResourceResolutionStatusType_Success :: ResourceResolutionStatusType
pattern ResourceResolutionStatusType_Success = ResourceResolutionStatusType' "Success"

{-# COMPLETE
  ResourceResolutionStatusType_Failed,
  ResourceResolutionStatusType_InProgress,
  ResourceResolutionStatusType_Pending,
  ResourceResolutionStatusType_Success,
  ResourceResolutionStatusType'
  #-}
