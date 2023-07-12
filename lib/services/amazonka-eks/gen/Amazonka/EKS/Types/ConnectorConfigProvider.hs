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
-- Module      : Amazonka.EKS.Types.ConnectorConfigProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ConnectorConfigProvider
  ( ConnectorConfigProvider
      ( ..,
        ConnectorConfigProvider_AKS,
        ConnectorConfigProvider_ANTHOS,
        ConnectorConfigProvider_EC2,
        ConnectorConfigProvider_EKS_ANYWHERE,
        ConnectorConfigProvider_GKE,
        ConnectorConfigProvider_OPENSHIFT,
        ConnectorConfigProvider_OTHER,
        ConnectorConfigProvider_RANCHER,
        ConnectorConfigProvider_TANZU
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectorConfigProvider = ConnectorConfigProvider'
  { fromConnectorConfigProvider ::
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

pattern ConnectorConfigProvider_AKS :: ConnectorConfigProvider
pattern ConnectorConfigProvider_AKS = ConnectorConfigProvider' "AKS"

pattern ConnectorConfigProvider_ANTHOS :: ConnectorConfigProvider
pattern ConnectorConfigProvider_ANTHOS = ConnectorConfigProvider' "ANTHOS"

pattern ConnectorConfigProvider_EC2 :: ConnectorConfigProvider
pattern ConnectorConfigProvider_EC2 = ConnectorConfigProvider' "EC2"

pattern ConnectorConfigProvider_EKS_ANYWHERE :: ConnectorConfigProvider
pattern ConnectorConfigProvider_EKS_ANYWHERE = ConnectorConfigProvider' "EKS_ANYWHERE"

pattern ConnectorConfigProvider_GKE :: ConnectorConfigProvider
pattern ConnectorConfigProvider_GKE = ConnectorConfigProvider' "GKE"

pattern ConnectorConfigProvider_OPENSHIFT :: ConnectorConfigProvider
pattern ConnectorConfigProvider_OPENSHIFT = ConnectorConfigProvider' "OPENSHIFT"

pattern ConnectorConfigProvider_OTHER :: ConnectorConfigProvider
pattern ConnectorConfigProvider_OTHER = ConnectorConfigProvider' "OTHER"

pattern ConnectorConfigProvider_RANCHER :: ConnectorConfigProvider
pattern ConnectorConfigProvider_RANCHER = ConnectorConfigProvider' "RANCHER"

pattern ConnectorConfigProvider_TANZU :: ConnectorConfigProvider
pattern ConnectorConfigProvider_TANZU = ConnectorConfigProvider' "TANZU"

{-# COMPLETE
  ConnectorConfigProvider_AKS,
  ConnectorConfigProvider_ANTHOS,
  ConnectorConfigProvider_EC2,
  ConnectorConfigProvider_EKS_ANYWHERE,
  ConnectorConfigProvider_GKE,
  ConnectorConfigProvider_OPENSHIFT,
  ConnectorConfigProvider_OTHER,
  ConnectorConfigProvider_RANCHER,
  ConnectorConfigProvider_TANZU,
  ConnectorConfigProvider'
  #-}
