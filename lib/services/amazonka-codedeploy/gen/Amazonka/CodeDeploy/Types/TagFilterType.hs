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
-- Module      : Amazonka.CodeDeploy.Types.TagFilterType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TagFilterType
  ( TagFilterType
      ( ..,
        TagFilterType_KEY_AND_VALUE,
        TagFilterType_KEY_ONLY,
        TagFilterType_VALUE_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TagFilterType = TagFilterType'
  { fromTagFilterType ::
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
