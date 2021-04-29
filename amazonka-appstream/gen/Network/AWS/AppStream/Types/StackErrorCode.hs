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

import qualified Network.AWS.Prelude as Prelude

newtype StackErrorCode = StackErrorCode'
  { fromStackErrorCode ::
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

pattern StackErrorCode_INTERNAL_SERVICE_ERROR :: StackErrorCode
pattern StackErrorCode_INTERNAL_SERVICE_ERROR = StackErrorCode' "INTERNAL_SERVICE_ERROR"

pattern StackErrorCode_STORAGE_CONNECTOR_ERROR :: StackErrorCode
pattern StackErrorCode_STORAGE_CONNECTOR_ERROR = StackErrorCode' "STORAGE_CONNECTOR_ERROR"

{-# COMPLETE
  StackErrorCode_INTERNAL_SERVICE_ERROR,
  StackErrorCode_STORAGE_CONNECTOR_ERROR,
  StackErrorCode'
  #-}
