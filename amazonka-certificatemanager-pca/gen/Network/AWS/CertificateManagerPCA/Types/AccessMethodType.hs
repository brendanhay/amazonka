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
-- Module      : Network.AWS.CertificateManagerPCA.Types.AccessMethodType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.AccessMethodType
  ( AccessMethodType
      ( ..,
        AccessMethodType_CA_REPOSITORY,
        AccessMethodType_RESOURCE_PKI_MANIFEST,
        AccessMethodType_RESOURCE_PKI_NOTIFY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AccessMethodType = AccessMethodType'
  { fromAccessMethodType ::
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

pattern AccessMethodType_CA_REPOSITORY :: AccessMethodType
pattern AccessMethodType_CA_REPOSITORY = AccessMethodType' "CA_REPOSITORY"

pattern AccessMethodType_RESOURCE_PKI_MANIFEST :: AccessMethodType
pattern AccessMethodType_RESOURCE_PKI_MANIFEST = AccessMethodType' "RESOURCE_PKI_MANIFEST"

pattern AccessMethodType_RESOURCE_PKI_NOTIFY :: AccessMethodType
pattern AccessMethodType_RESOURCE_PKI_NOTIFY = AccessMethodType' "RESOURCE_PKI_NOTIFY"

{-# COMPLETE
  AccessMethodType_CA_REPOSITORY,
  AccessMethodType_RESOURCE_PKI_MANIFEST,
  AccessMethodType_RESOURCE_PKI_NOTIFY,
  AccessMethodType'
  #-}
