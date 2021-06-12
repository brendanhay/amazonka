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
-- Module      : Network.AWS.S3.Types.IntelligentTieringStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringStatus
  ( IntelligentTieringStatus
      ( ..,
        IntelligentTieringStatus_Disabled,
        IntelligentTieringStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

newtype IntelligentTieringStatus = IntelligentTieringStatus'
  { fromIntelligentTieringStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern IntelligentTieringStatus_Disabled :: IntelligentTieringStatus
pattern IntelligentTieringStatus_Disabled = IntelligentTieringStatus' "Disabled"

pattern IntelligentTieringStatus_Enabled :: IntelligentTieringStatus
pattern IntelligentTieringStatus_Enabled = IntelligentTieringStatus' "Enabled"

{-# COMPLETE
  IntelligentTieringStatus_Disabled,
  IntelligentTieringStatus_Enabled,
  IntelligentTieringStatus'
  #-}
