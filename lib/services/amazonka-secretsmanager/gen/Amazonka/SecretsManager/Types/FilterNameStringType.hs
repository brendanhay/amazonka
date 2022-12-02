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
-- Module      : Amazonka.SecretsManager.Types.FilterNameStringType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.FilterNameStringType
  ( FilterNameStringType
      ( ..,
        FilterNameStringType_All,
        FilterNameStringType_Description,
        FilterNameStringType_Name,
        FilterNameStringType_Primary_region,
        FilterNameStringType_Tag_key,
        FilterNameStringType_Tag_value
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FilterNameStringType = FilterNameStringType'
  { fromFilterNameStringType ::
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

pattern FilterNameStringType_All :: FilterNameStringType
pattern FilterNameStringType_All = FilterNameStringType' "all"

pattern FilterNameStringType_Description :: FilterNameStringType
pattern FilterNameStringType_Description = FilterNameStringType' "description"

pattern FilterNameStringType_Name :: FilterNameStringType
pattern FilterNameStringType_Name = FilterNameStringType' "name"

pattern FilterNameStringType_Primary_region :: FilterNameStringType
pattern FilterNameStringType_Primary_region = FilterNameStringType' "primary-region"

pattern FilterNameStringType_Tag_key :: FilterNameStringType
pattern FilterNameStringType_Tag_key = FilterNameStringType' "tag-key"

pattern FilterNameStringType_Tag_value :: FilterNameStringType
pattern FilterNameStringType_Tag_value = FilterNameStringType' "tag-value"

{-# COMPLETE
  FilterNameStringType_All,
  FilterNameStringType_Description,
  FilterNameStringType_Name,
  FilterNameStringType_Primary_region,
  FilterNameStringType_Tag_key,
  FilterNameStringType_Tag_value,
  FilterNameStringType'
  #-}
