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
-- Module      : Amazonka.EKS.Types.UpdateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.UpdateType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UpdateType = UpdateType'
  { fromUpdateType ::
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
