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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataFieldDataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataFieldDataType
  ( CodegenGenericDataFieldDataType
      ( ..,
        CodegenGenericDataFieldDataType_AWSDate,
        CodegenGenericDataFieldDataType_AWSDateTime,
        CodegenGenericDataFieldDataType_AWSEmail,
        CodegenGenericDataFieldDataType_AWSIPAddress,
        CodegenGenericDataFieldDataType_AWSJSON,
        CodegenGenericDataFieldDataType_AWSPhone,
        CodegenGenericDataFieldDataType_AWSTime,
        CodegenGenericDataFieldDataType_AWSTimestamp,
        CodegenGenericDataFieldDataType_AWSURL,
        CodegenGenericDataFieldDataType_Boolean,
        CodegenGenericDataFieldDataType_Enum,
        CodegenGenericDataFieldDataType_Float,
        CodegenGenericDataFieldDataType_ID,
        CodegenGenericDataFieldDataType_Int,
        CodegenGenericDataFieldDataType_Model,
        CodegenGenericDataFieldDataType_NonModel,
        CodegenGenericDataFieldDataType_String
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CodegenGenericDataFieldDataType = CodegenGenericDataFieldDataType'
  { fromCodegenGenericDataFieldDataType ::
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

pattern CodegenGenericDataFieldDataType_AWSDate :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSDate = CodegenGenericDataFieldDataType' "AWSDate"

pattern CodegenGenericDataFieldDataType_AWSDateTime :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSDateTime = CodegenGenericDataFieldDataType' "AWSDateTime"

pattern CodegenGenericDataFieldDataType_AWSEmail :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSEmail = CodegenGenericDataFieldDataType' "AWSEmail"

pattern CodegenGenericDataFieldDataType_AWSIPAddress :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSIPAddress = CodegenGenericDataFieldDataType' "AWSIPAddress"

pattern CodegenGenericDataFieldDataType_AWSJSON :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSJSON = CodegenGenericDataFieldDataType' "AWSJSON"

pattern CodegenGenericDataFieldDataType_AWSPhone :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSPhone = CodegenGenericDataFieldDataType' "AWSPhone"

pattern CodegenGenericDataFieldDataType_AWSTime :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSTime = CodegenGenericDataFieldDataType' "AWSTime"

pattern CodegenGenericDataFieldDataType_AWSTimestamp :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSTimestamp = CodegenGenericDataFieldDataType' "AWSTimestamp"

pattern CodegenGenericDataFieldDataType_AWSURL :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_AWSURL = CodegenGenericDataFieldDataType' "AWSURL"

pattern CodegenGenericDataFieldDataType_Boolean :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_Boolean = CodegenGenericDataFieldDataType' "Boolean"

pattern CodegenGenericDataFieldDataType_Enum :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_Enum = CodegenGenericDataFieldDataType' "Enum"

pattern CodegenGenericDataFieldDataType_Float :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_Float = CodegenGenericDataFieldDataType' "Float"

pattern CodegenGenericDataFieldDataType_ID :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_ID = CodegenGenericDataFieldDataType' "ID"

pattern CodegenGenericDataFieldDataType_Int :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_Int = CodegenGenericDataFieldDataType' "Int"

pattern CodegenGenericDataFieldDataType_Model :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_Model = CodegenGenericDataFieldDataType' "Model"

pattern CodegenGenericDataFieldDataType_NonModel :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_NonModel = CodegenGenericDataFieldDataType' "NonModel"

pattern CodegenGenericDataFieldDataType_String :: CodegenGenericDataFieldDataType
pattern CodegenGenericDataFieldDataType_String = CodegenGenericDataFieldDataType' "String"

{-# COMPLETE
  CodegenGenericDataFieldDataType_AWSDate,
  CodegenGenericDataFieldDataType_AWSDateTime,
  CodegenGenericDataFieldDataType_AWSEmail,
  CodegenGenericDataFieldDataType_AWSIPAddress,
  CodegenGenericDataFieldDataType_AWSJSON,
  CodegenGenericDataFieldDataType_AWSPhone,
  CodegenGenericDataFieldDataType_AWSTime,
  CodegenGenericDataFieldDataType_AWSTimestamp,
  CodegenGenericDataFieldDataType_AWSURL,
  CodegenGenericDataFieldDataType_Boolean,
  CodegenGenericDataFieldDataType_Enum,
  CodegenGenericDataFieldDataType_Float,
  CodegenGenericDataFieldDataType_ID,
  CodegenGenericDataFieldDataType_Int,
  CodegenGenericDataFieldDataType_Model,
  CodegenGenericDataFieldDataType_NonModel,
  CodegenGenericDataFieldDataType_String,
  CodegenGenericDataFieldDataType'
  #-}
