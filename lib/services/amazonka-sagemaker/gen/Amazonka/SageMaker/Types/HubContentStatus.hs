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
-- Module      : Amazonka.SageMaker.Types.HubContentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubContentStatus
  ( HubContentStatus
      ( ..,
        HubContentStatus_Available,
        HubContentStatus_DeleteFailed,
        HubContentStatus_Deleting,
        HubContentStatus_ImportFailed,
        HubContentStatus_Importing
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HubContentStatus = HubContentStatus'
  { fromHubContentStatus ::
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

pattern HubContentStatus_Available :: HubContentStatus
pattern HubContentStatus_Available = HubContentStatus' "Available"

pattern HubContentStatus_DeleteFailed :: HubContentStatus
pattern HubContentStatus_DeleteFailed = HubContentStatus' "DeleteFailed"

pattern HubContentStatus_Deleting :: HubContentStatus
pattern HubContentStatus_Deleting = HubContentStatus' "Deleting"

pattern HubContentStatus_ImportFailed :: HubContentStatus
pattern HubContentStatus_ImportFailed = HubContentStatus' "ImportFailed"

pattern HubContentStatus_Importing :: HubContentStatus
pattern HubContentStatus_Importing = HubContentStatus' "Importing"

{-# COMPLETE
  HubContentStatus_Available,
  HubContentStatus_DeleteFailed,
  HubContentStatus_Deleting,
  HubContentStatus_ImportFailed,
  HubContentStatus_Importing,
  HubContentStatus'
  #-}
