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
-- Module      : Network.AWS.AppStream.Types.StackAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackAttribute
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
        StackAttribute_THEME_NAME,
        StackAttribute_USER_SETTINGS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StackAttribute = StackAttribute'
  { fromStackAttribute ::
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
  StackAttribute_THEME_NAME,
  StackAttribute_USER_SETTINGS,
  StackAttribute'
  #-}
