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
-- Module      : Network.AWS.AppStream.Types.StackErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackErrorCode
  ( StackErrorCode
      ( ..,
        StackErrorCode_INTERNAL_SERVICE_ERROR,
        StackErrorCode_STORAGE_CONNECTOR_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StackErrorCode = StackErrorCode'
  { fromStackErrorCode ::
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

pattern StackErrorCode_INTERNAL_SERVICE_ERROR :: StackErrorCode
pattern StackErrorCode_INTERNAL_SERVICE_ERROR = StackErrorCode' "INTERNAL_SERVICE_ERROR"

pattern StackErrorCode_STORAGE_CONNECTOR_ERROR :: StackErrorCode
pattern StackErrorCode_STORAGE_CONNECTOR_ERROR = StackErrorCode' "STORAGE_CONNECTOR_ERROR"

{-# COMPLETE
  StackErrorCode_INTERNAL_SERVICE_ERROR,
  StackErrorCode_STORAGE_CONNECTOR_ERROR,
  StackErrorCode'
  #-}
