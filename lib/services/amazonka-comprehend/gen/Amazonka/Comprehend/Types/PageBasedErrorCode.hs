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
-- Module      : Amazonka.Comprehend.Types.PageBasedErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PageBasedErrorCode
  ( PageBasedErrorCode
      ( ..,
        PageBasedErrorCode_INTERNAL_SERVER_ERROR,
        PageBasedErrorCode_PAGE_CHARACTERS_EXCEEDED,
        PageBasedErrorCode_PAGE_SIZE_EXCEEDED,
        PageBasedErrorCode_TEXTRACT_BAD_PAGE,
        PageBasedErrorCode_TEXTRACT_PROVISIONED_THROUGHPUT_EXCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PageBasedErrorCode = PageBasedErrorCode'
  { fromPageBasedErrorCode ::
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

pattern PageBasedErrorCode_INTERNAL_SERVER_ERROR :: PageBasedErrorCode
pattern PageBasedErrorCode_INTERNAL_SERVER_ERROR = PageBasedErrorCode' "INTERNAL_SERVER_ERROR"

pattern PageBasedErrorCode_PAGE_CHARACTERS_EXCEEDED :: PageBasedErrorCode
pattern PageBasedErrorCode_PAGE_CHARACTERS_EXCEEDED = PageBasedErrorCode' "PAGE_CHARACTERS_EXCEEDED"

pattern PageBasedErrorCode_PAGE_SIZE_EXCEEDED :: PageBasedErrorCode
pattern PageBasedErrorCode_PAGE_SIZE_EXCEEDED = PageBasedErrorCode' "PAGE_SIZE_EXCEEDED"

pattern PageBasedErrorCode_TEXTRACT_BAD_PAGE :: PageBasedErrorCode
pattern PageBasedErrorCode_TEXTRACT_BAD_PAGE = PageBasedErrorCode' "TEXTRACT_BAD_PAGE"

pattern PageBasedErrorCode_TEXTRACT_PROVISIONED_THROUGHPUT_EXCEEDED :: PageBasedErrorCode
pattern PageBasedErrorCode_TEXTRACT_PROVISIONED_THROUGHPUT_EXCEEDED = PageBasedErrorCode' "TEXTRACT_PROVISIONED_THROUGHPUT_EXCEEDED"

{-# COMPLETE
  PageBasedErrorCode_INTERNAL_SERVER_ERROR,
  PageBasedErrorCode_PAGE_CHARACTERS_EXCEEDED,
  PageBasedErrorCode_PAGE_SIZE_EXCEEDED,
  PageBasedErrorCode_TEXTRACT_BAD_PAGE,
  PageBasedErrorCode_TEXTRACT_PROVISIONED_THROUGHPUT_EXCEEDED,
  PageBasedErrorCode'
  #-}
