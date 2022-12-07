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
-- Module      : Amazonka.CodePipeline.Types.ActionConfigurationPropertyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionConfigurationPropertyType
  ( ActionConfigurationPropertyType
      ( ..,
        ActionConfigurationPropertyType_Boolean,
        ActionConfigurationPropertyType_Number,
        ActionConfigurationPropertyType_String
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionConfigurationPropertyType = ActionConfigurationPropertyType'
  { fromActionConfigurationPropertyType ::
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

pattern ActionConfigurationPropertyType_Boolean :: ActionConfigurationPropertyType
pattern ActionConfigurationPropertyType_Boolean = ActionConfigurationPropertyType' "Boolean"

pattern ActionConfigurationPropertyType_Number :: ActionConfigurationPropertyType
pattern ActionConfigurationPropertyType_Number = ActionConfigurationPropertyType' "Number"

pattern ActionConfigurationPropertyType_String :: ActionConfigurationPropertyType
pattern ActionConfigurationPropertyType_String = ActionConfigurationPropertyType' "String"

{-# COMPLETE
  ActionConfigurationPropertyType_Boolean,
  ActionConfigurationPropertyType_Number,
  ActionConfigurationPropertyType_String,
  ActionConfigurationPropertyType'
  #-}
