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
-- Module      : Network.AWS.AppStream.Types.VisibilityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.VisibilityType
  ( VisibilityType
      ( ..,
        VisibilityType_PRIVATE,
        VisibilityType_PUBLIC,
        VisibilityType_SHARED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype VisibilityType = VisibilityType'
  { fromVisibilityType ::
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

pattern VisibilityType_PRIVATE :: VisibilityType
pattern VisibilityType_PRIVATE = VisibilityType' "PRIVATE"

pattern VisibilityType_PUBLIC :: VisibilityType
pattern VisibilityType_PUBLIC = VisibilityType' "PUBLIC"

pattern VisibilityType_SHARED :: VisibilityType
pattern VisibilityType_SHARED = VisibilityType' "SHARED"

{-# COMPLETE
  VisibilityType_PRIVATE,
  VisibilityType_PUBLIC,
  VisibilityType_SHARED,
  VisibilityType'
  #-}
