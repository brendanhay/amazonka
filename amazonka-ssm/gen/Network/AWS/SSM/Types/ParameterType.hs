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
-- Module      : Network.AWS.SSM.Types.ParameterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterType
  ( ParameterType
      ( ..,
        ParameterType_SecureString,
        ParameterType_String,
        ParameterType_StringList
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ParameterType = ParameterType'
  { fromParameterType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ParameterType_SecureString :: ParameterType
pattern ParameterType_SecureString = ParameterType' "SecureString"

pattern ParameterType_String :: ParameterType
pattern ParameterType_String = ParameterType' "String"

pattern ParameterType_StringList :: ParameterType
pattern ParameterType_StringList = ParameterType' "StringList"

{-# COMPLETE
  ParameterType_SecureString,
  ParameterType_String,
  ParameterType_StringList,
  ParameterType'
  #-}
