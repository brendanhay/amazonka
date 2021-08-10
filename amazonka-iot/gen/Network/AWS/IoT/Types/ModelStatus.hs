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
-- Module      : Network.AWS.IoT.Types.ModelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_ACTIVE,
        ModelStatus_EXPIRED,
        ModelStatus_PENDING_BUILD
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ModelStatus = ModelStatus'
  { fromModelStatus ::
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

pattern ModelStatus_ACTIVE :: ModelStatus
pattern ModelStatus_ACTIVE = ModelStatus' "ACTIVE"

pattern ModelStatus_EXPIRED :: ModelStatus
pattern ModelStatus_EXPIRED = ModelStatus' "EXPIRED"

pattern ModelStatus_PENDING_BUILD :: ModelStatus
pattern ModelStatus_PENDING_BUILD = ModelStatus' "PENDING_BUILD"

{-# COMPLETE
  ModelStatus_ACTIVE,
  ModelStatus_EXPIRED,
  ModelStatus_PENDING_BUILD,
  ModelStatus'
  #-}
