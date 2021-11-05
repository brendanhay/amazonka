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
-- Module      : Network.AWS.SSOAdmin.Types.StatusValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSOAdmin.Types.StatusValues
  ( StatusValues
      ( ..,
        StatusValues_FAILED,
        StatusValues_IN_PROGRESS,
        StatusValues_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StatusValues = StatusValues'
  { fromStatusValues ::
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

pattern StatusValues_FAILED :: StatusValues
pattern StatusValues_FAILED = StatusValues' "FAILED"

pattern StatusValues_IN_PROGRESS :: StatusValues
pattern StatusValues_IN_PROGRESS = StatusValues' "IN_PROGRESS"

pattern StatusValues_SUCCEEDED :: StatusValues
pattern StatusValues_SUCCEEDED = StatusValues' "SUCCEEDED"

{-# COMPLETE
  StatusValues_FAILED,
  StatusValues_IN_PROGRESS,
  StatusValues_SUCCEEDED,
  StatusValues'
  #-}
