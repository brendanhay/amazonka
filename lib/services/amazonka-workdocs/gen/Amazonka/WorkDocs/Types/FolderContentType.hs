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
-- Module      : Amazonka.WorkDocs.Types.FolderContentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.FolderContentType
  ( FolderContentType
      ( ..,
        FolderContentType_ALL,
        FolderContentType_DOCUMENT,
        FolderContentType_FOLDER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FolderContentType = FolderContentType'
  { fromFolderContentType ::
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

pattern FolderContentType_ALL :: FolderContentType
pattern FolderContentType_ALL = FolderContentType' "ALL"

pattern FolderContentType_DOCUMENT :: FolderContentType
pattern FolderContentType_DOCUMENT = FolderContentType' "DOCUMENT"

pattern FolderContentType_FOLDER :: FolderContentType
pattern FolderContentType_FOLDER = FolderContentType' "FOLDER"

{-# COMPLETE
  FolderContentType_ALL,
  FolderContentType_DOCUMENT,
  FolderContentType_FOLDER,
  FolderContentType'
  #-}
