{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype TagFilterType = TagFilterType'
  { fromTagFilterType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
