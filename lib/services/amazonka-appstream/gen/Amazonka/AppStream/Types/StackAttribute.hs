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
-- Module      : Amazonka.AppStream.Types.StackAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.StackAttribute
  ( StackAttribute
      ( ..,
        StackAttribute_ACCESS_ENDPOINTS,
        StackAttribute_EMBED_HOST_DOMAINS,
        StackAttribute_FEEDBACK_URL,
        StackAttribute_IAM_ROLE_ARN,
        StackAttribute_REDIRECT_URL,
        StackAttribute_STORAGE_CONNECTORS,
        StackAttribute_STORAGE_CONNECTOR_GOOGLE_DRIVE,
        StackAttribute_STORAGE_CONNECTOR_HOMEFOLDERS,
        StackAttribute_STORAGE_CONNECTOR_ONE_DRIVE,
        StackAttribute_STREAMING_EXPERIENCE_SETTINGS,
        StackAttribute_THEME_NAME,
        StackAttribute_USER_SETTINGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackAttribute = StackAttribute'
  { fromStackAttribute ::
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

pattern StackAttribute_ACCESS_ENDPOINTS :: StackAttribute
pattern StackAttribute_ACCESS_ENDPOINTS = StackAttribute' "ACCESS_ENDPOINTS"

pattern StackAttribute_EMBED_HOST_DOMAINS :: StackAttribute
pattern StackAttribute_EMBED_HOST_DOMAINS = StackAttribute' "EMBED_HOST_DOMAINS"

pattern StackAttribute_FEEDBACK_URL :: StackAttribute
pattern StackAttribute_FEEDBACK_URL = StackAttribute' "FEEDBACK_URL"

pattern StackAttribute_IAM_ROLE_ARN :: StackAttribute
pattern StackAttribute_IAM_ROLE_ARN = StackAttribute' "IAM_ROLE_ARN"

pattern StackAttribute_REDIRECT_URL :: StackAttribute
pattern StackAttribute_REDIRECT_URL = StackAttribute' "REDIRECT_URL"

pattern StackAttribute_STORAGE_CONNECTORS :: StackAttribute
pattern StackAttribute_STORAGE_CONNECTORS = StackAttribute' "STORAGE_CONNECTORS"

pattern StackAttribute_STORAGE_CONNECTOR_GOOGLE_DRIVE :: StackAttribute
pattern StackAttribute_STORAGE_CONNECTOR_GOOGLE_DRIVE = StackAttribute' "STORAGE_CONNECTOR_GOOGLE_DRIVE"

pattern StackAttribute_STORAGE_CONNECTOR_HOMEFOLDERS :: StackAttribute
pattern StackAttribute_STORAGE_CONNECTOR_HOMEFOLDERS = StackAttribute' "STORAGE_CONNECTOR_HOMEFOLDERS"

pattern StackAttribute_STORAGE_CONNECTOR_ONE_DRIVE :: StackAttribute
pattern StackAttribute_STORAGE_CONNECTOR_ONE_DRIVE = StackAttribute' "STORAGE_CONNECTOR_ONE_DRIVE"

pattern StackAttribute_STREAMING_EXPERIENCE_SETTINGS :: StackAttribute
pattern StackAttribute_STREAMING_EXPERIENCE_SETTINGS = StackAttribute' "STREAMING_EXPERIENCE_SETTINGS"

pattern StackAttribute_THEME_NAME :: StackAttribute
pattern StackAttribute_THEME_NAME = StackAttribute' "THEME_NAME"

pattern StackAttribute_USER_SETTINGS :: StackAttribute
pattern StackAttribute_USER_SETTINGS = StackAttribute' "USER_SETTINGS"

{-# COMPLETE
  StackAttribute_ACCESS_ENDPOINTS,
  StackAttribute_EMBED_HOST_DOMAINS,
  StackAttribute_FEEDBACK_URL,
  StackAttribute_IAM_ROLE_ARN,
  StackAttribute_REDIRECT_URL,
  StackAttribute_STORAGE_CONNECTORS,
  StackAttribute_STORAGE_CONNECTOR_GOOGLE_DRIVE,
  StackAttribute_STORAGE_CONNECTOR_HOMEFOLDERS,
  StackAttribute_STORAGE_CONNECTOR_ONE_DRIVE,
  StackAttribute_STREAMING_EXPERIENCE_SETTINGS,
  StackAttribute_THEME_NAME,
  StackAttribute_USER_SETTINGS,
  StackAttribute'
  #-}
