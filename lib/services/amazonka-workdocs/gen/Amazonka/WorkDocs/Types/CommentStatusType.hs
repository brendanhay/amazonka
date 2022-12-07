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
-- Module      : Amazonka.WorkDocs.Types.CommentStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.CommentStatusType
  ( CommentStatusType
      ( ..,
        CommentStatusType_DELETED,
        CommentStatusType_DRAFT,
        CommentStatusType_PUBLISHED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CommentStatusType = CommentStatusType'
  { fromCommentStatusType ::
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
