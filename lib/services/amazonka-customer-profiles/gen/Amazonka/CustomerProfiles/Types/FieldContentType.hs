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
-- Module      : Amazonka.CustomerProfiles.Types.FieldContentType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.FieldContentType
  ( FieldContentType
      ( ..,
        FieldContentType_EMAIL_ADDRESS,
        FieldContentType_NAME,
        FieldContentType_NUMBER,
        FieldContentType_PHONE_NUMBER,
        FieldContentType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FieldContentType = FieldContentType'
  { fromFieldContentType ::
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

pattern FieldContentType_EMAIL_ADDRESS :: FieldContentType
pattern FieldContentType_EMAIL_ADDRESS = FieldContentType' "EMAIL_ADDRESS"

pattern FieldContentType_NAME :: FieldContentType
pattern FieldContentType_NAME = FieldContentType' "NAME"

pattern FieldContentType_NUMBER :: FieldContentType
pattern FieldContentType_NUMBER = FieldContentType' "NUMBER"

pattern FieldContentType_PHONE_NUMBER :: FieldContentType
pattern FieldContentType_PHONE_NUMBER = FieldContentType' "PHONE_NUMBER"

pattern FieldContentType_STRING :: FieldContentType
pattern FieldContentType_STRING = FieldContentType' "STRING"

{-# COMPLETE
  FieldContentType_EMAIL_ADDRESS,
  FieldContentType_NAME,
  FieldContentType_NUMBER,
  FieldContentType_PHONE_NUMBER,
  FieldContentType_STRING,
  FieldContentType'
  #-}
