{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
  ( ActionConfigurationPropertyType
      ( ..,
        ActionConfigurationPropertyType_Boolean,
        ActionConfigurationPropertyType_Number,
        ActionConfigurationPropertyType_String
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ActionConfigurationPropertyType = ActionConfigurationPropertyType'
  { fromActionConfigurationPropertyType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
