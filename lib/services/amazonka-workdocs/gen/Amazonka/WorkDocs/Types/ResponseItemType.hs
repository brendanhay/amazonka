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
-- Module      : Amazonka.WorkDocs.Types.ResponseItemType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResponseItemType
  ( ResponseItemType
      ( ..,
        ResponseItemType_COMMENT,
        ResponseItemType_DOCUMENT,
        ResponseItemType_DOCUMENT_VERSION,
        ResponseItemType_FOLDER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResponseItemType = ResponseItemType'
  { fromResponseItemType ::
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

pattern ResponseItemType_COMMENT :: ResponseItemType
pattern ResponseItemType_COMMENT = ResponseItemType' "COMMENT"

pattern ResponseItemType_DOCUMENT :: ResponseItemType
pattern ResponseItemType_DOCUMENT = ResponseItemType' "DOCUMENT"

pattern ResponseItemType_DOCUMENT_VERSION :: ResponseItemType
pattern ResponseItemType_DOCUMENT_VERSION = ResponseItemType' "DOCUMENT_VERSION"

pattern ResponseItemType_FOLDER :: ResponseItemType
pattern ResponseItemType_FOLDER = ResponseItemType' "FOLDER"

{-# COMPLETE
  ResponseItemType_COMMENT,
  ResponseItemType_DOCUMENT,
  ResponseItemType_DOCUMENT_VERSION,
  ResponseItemType_FOLDER,
  ResponseItemType'
  #-}
