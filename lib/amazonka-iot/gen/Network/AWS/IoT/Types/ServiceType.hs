{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServiceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ServiceType
  ( ServiceType
      ( ServiceType',
        ServiceTypeData,
        ServiceTypeCredentialProvider,
        ServiceTypeJobs,
        fromServiceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ServiceType = ServiceType' {fromServiceType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ServiceTypeData :: ServiceType
pattern ServiceTypeData = ServiceType' "DATA"

pattern ServiceTypeCredentialProvider :: ServiceType
pattern ServiceTypeCredentialProvider = ServiceType' "CREDENTIAL_PROVIDER"

pattern ServiceTypeJobs :: ServiceType
pattern ServiceTypeJobs = ServiceType' "JOBS"

{-# COMPLETE
  ServiceTypeData,
  ServiceTypeCredentialProvider,
  ServiceTypeJobs,
  ServiceType'
  #-}
