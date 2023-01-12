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
-- Module      : Amazonka.WAFV2.Types.ResponseContentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ResponseContentType
  ( ResponseContentType
      ( ..,
        ResponseContentType_APPLICATION_JSON,
        ResponseContentType_TEXT_HTML,
        ResponseContentType_TEXT_PLAIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResponseContentType = ResponseContentType'
  { fromResponseContentType ::
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

pattern ResponseContentType_APPLICATION_JSON :: ResponseContentType
pattern ResponseContentType_APPLICATION_JSON = ResponseContentType' "APPLICATION_JSON"

pattern ResponseContentType_TEXT_HTML :: ResponseContentType
pattern ResponseContentType_TEXT_HTML = ResponseContentType' "TEXT_HTML"

pattern ResponseContentType_TEXT_PLAIN :: ResponseContentType
pattern ResponseContentType_TEXT_PLAIN = ResponseContentType' "TEXT_PLAIN"

{-# COMPLETE
  ResponseContentType_APPLICATION_JSON,
  ResponseContentType_TEXT_HTML,
  ResponseContentType_TEXT_PLAIN,
  ResponseContentType'
  #-}
