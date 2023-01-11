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
-- Module      : Amazonka.Connect.Types.ReferenceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ReferenceType
  ( ReferenceType
      ( ..,
        ReferenceType_ATTACHMENT,
        ReferenceType_DATE,
        ReferenceType_EMAIL,
        ReferenceType_NUMBER,
        ReferenceType_STRING,
        ReferenceType_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReferenceType = ReferenceType'
  { fromReferenceType ::
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

pattern ReferenceType_ATTACHMENT :: ReferenceType
pattern ReferenceType_ATTACHMENT = ReferenceType' "ATTACHMENT"

pattern ReferenceType_DATE :: ReferenceType
pattern ReferenceType_DATE = ReferenceType' "DATE"

pattern ReferenceType_EMAIL :: ReferenceType
pattern ReferenceType_EMAIL = ReferenceType' "EMAIL"

pattern ReferenceType_NUMBER :: ReferenceType
pattern ReferenceType_NUMBER = ReferenceType' "NUMBER"

pattern ReferenceType_STRING :: ReferenceType
pattern ReferenceType_STRING = ReferenceType' "STRING"

pattern ReferenceType_URL :: ReferenceType
pattern ReferenceType_URL = ReferenceType' "URL"

{-# COMPLETE
  ReferenceType_ATTACHMENT,
  ReferenceType_DATE,
  ReferenceType_EMAIL,
  ReferenceType_NUMBER,
  ReferenceType_STRING,
  ReferenceType_URL,
  ReferenceType'
  #-}
