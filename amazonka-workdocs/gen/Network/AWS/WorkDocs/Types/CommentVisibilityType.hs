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
-- Module      : Network.AWS.WorkDocs.Types.CommentVisibilityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentVisibilityType
  ( CommentVisibilityType
      ( ..,
        CommentVisibilityType_PRIVATE,
        CommentVisibilityType_PUBLIC
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CommentVisibilityType = CommentVisibilityType'
  { fromCommentVisibilityType ::
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

pattern CommentVisibilityType_PRIVATE :: CommentVisibilityType
pattern CommentVisibilityType_PRIVATE = CommentVisibilityType' "PRIVATE"

pattern CommentVisibilityType_PUBLIC :: CommentVisibilityType
pattern CommentVisibilityType_PUBLIC = CommentVisibilityType' "PUBLIC"

{-# COMPLETE
  CommentVisibilityType_PRIVATE,
  CommentVisibilityType_PUBLIC,
  CommentVisibilityType'
  #-}
