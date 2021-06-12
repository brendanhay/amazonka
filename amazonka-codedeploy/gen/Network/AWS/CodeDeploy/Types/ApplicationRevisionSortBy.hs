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
-- Module      : Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy
  ( ApplicationRevisionSortBy
      ( ..,
        ApplicationRevisionSortBy_FirstUsedTime,
        ApplicationRevisionSortBy_LastUsedTime,
        ApplicationRevisionSortBy_RegisterTime
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ApplicationRevisionSortBy = ApplicationRevisionSortBy'
  { fromApplicationRevisionSortBy ::
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

pattern ApplicationRevisionSortBy_FirstUsedTime :: ApplicationRevisionSortBy
pattern ApplicationRevisionSortBy_FirstUsedTime = ApplicationRevisionSortBy' "firstUsedTime"

pattern ApplicationRevisionSortBy_LastUsedTime :: ApplicationRevisionSortBy
pattern ApplicationRevisionSortBy_LastUsedTime = ApplicationRevisionSortBy' "lastUsedTime"

pattern ApplicationRevisionSortBy_RegisterTime :: ApplicationRevisionSortBy
pattern ApplicationRevisionSortBy_RegisterTime = ApplicationRevisionSortBy' "registerTime"

{-# COMPLETE
  ApplicationRevisionSortBy_FirstUsedTime,
  ApplicationRevisionSortBy_LastUsedTime,
  ApplicationRevisionSortBy_RegisterTime,
  ApplicationRevisionSortBy'
  #-}
