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
-- Module      : Network.AWS.CodeDeploy.Types.TagFilterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TagFilterType
  ( TagFilterType
      ( ..,
        TagFilterType_KEY_AND_VALUE,
        TagFilterType_KEY_ONLY,
        TagFilterType_VALUE_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TagFilterType = TagFilterType'
  { fromTagFilterType ::
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

pattern TagFilterType_KEY_AND_VALUE :: TagFilterType
pattern TagFilterType_KEY_AND_VALUE = TagFilterType' "KEY_AND_VALUE"

pattern TagFilterType_KEY_ONLY :: TagFilterType
pattern TagFilterType_KEY_ONLY = TagFilterType' "KEY_ONLY"

pattern TagFilterType_VALUE_ONLY :: TagFilterType
pattern TagFilterType_VALUE_ONLY = TagFilterType' "VALUE_ONLY"

{-# COMPLETE
  TagFilterType_KEY_AND_VALUE,
  TagFilterType_KEY_ONLY,
  TagFilterType_VALUE_ONLY,
  TagFilterType'
  #-}
