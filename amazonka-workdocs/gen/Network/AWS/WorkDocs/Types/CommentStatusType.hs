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
-- Module      : Network.AWS.WorkDocs.Types.CommentStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentStatusType
  ( CommentStatusType
      ( ..,
        CommentStatusType_DELETED,
        CommentStatusType_DRAFT,
        CommentStatusType_PUBLISHED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CommentStatusType = CommentStatusType'
  { fromCommentStatusType ::
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

pattern CommentStatusType_DELETED :: CommentStatusType
pattern CommentStatusType_DELETED = CommentStatusType' "DELETED"

pattern CommentStatusType_DRAFT :: CommentStatusType
pattern CommentStatusType_DRAFT = CommentStatusType' "DRAFT"

pattern CommentStatusType_PUBLISHED :: CommentStatusType
pattern CommentStatusType_PUBLISHED = CommentStatusType' "PUBLISHED"

{-# COMPLETE
  CommentStatusType_DELETED,
  CommentStatusType_DRAFT,
  CommentStatusType_PUBLISHED,
  CommentStatusType'
  #-}
