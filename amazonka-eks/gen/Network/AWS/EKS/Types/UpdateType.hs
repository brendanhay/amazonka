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
-- Module      : Network.AWS.EKS.Types.UpdateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.UpdateType
  ( UpdateType
      ( ..,
        UpdateType_AddonUpdate,
        UpdateType_AssociateEncryptionConfig,
        UpdateType_AssociateIdentityProviderConfig,
        UpdateType_ConfigUpdate,
        UpdateType_DisassociateIdentityProviderConfig,
        UpdateType_EndpointAccessUpdate,
        UpdateType_LoggingUpdate,
        UpdateType_VersionUpdate
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UpdateType = UpdateType'
  { fromUpdateType ::
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

pattern UpdateType_AddonUpdate :: UpdateType
pattern UpdateType_AddonUpdate = UpdateType' "AddonUpdate"

pattern UpdateType_AssociateEncryptionConfig :: UpdateType
pattern UpdateType_AssociateEncryptionConfig = UpdateType' "AssociateEncryptionConfig"

pattern UpdateType_AssociateIdentityProviderConfig :: UpdateType
pattern UpdateType_AssociateIdentityProviderConfig = UpdateType' "AssociateIdentityProviderConfig"

pattern UpdateType_ConfigUpdate :: UpdateType
pattern UpdateType_ConfigUpdate = UpdateType' "ConfigUpdate"

pattern UpdateType_DisassociateIdentityProviderConfig :: UpdateType
pattern UpdateType_DisassociateIdentityProviderConfig = UpdateType' "DisassociateIdentityProviderConfig"

pattern UpdateType_EndpointAccessUpdate :: UpdateType
pattern UpdateType_EndpointAccessUpdate = UpdateType' "EndpointAccessUpdate"

pattern UpdateType_LoggingUpdate :: UpdateType
pattern UpdateType_LoggingUpdate = UpdateType' "LoggingUpdate"

pattern UpdateType_VersionUpdate :: UpdateType
pattern UpdateType_VersionUpdate = UpdateType' "VersionUpdate"

{-# COMPLETE
  UpdateType_AddonUpdate,
  UpdateType_AssociateEncryptionConfig,
  UpdateType_AssociateIdentityProviderConfig,
  UpdateType_ConfigUpdate,
  UpdateType_DisassociateIdentityProviderConfig,
  UpdateType_EndpointAccessUpdate,
  UpdateType_LoggingUpdate,
  UpdateType_VersionUpdate,
  UpdateType'
  #-}
