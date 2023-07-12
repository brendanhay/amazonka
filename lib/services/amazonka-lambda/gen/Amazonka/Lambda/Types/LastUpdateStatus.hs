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
-- Module      : Amazonka.Lambda.Types.LastUpdateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.LastUpdateStatus
  ( LastUpdateStatus
      ( ..,
        LastUpdateStatus_Failed,
        LastUpdateStatus_InProgress,
        LastUpdateStatus_Successful
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LastUpdateStatus = LastUpdateStatus'
  { fromLastUpdateStatus ::
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

pattern LastUpdateStatus_Failed :: LastUpdateStatus
pattern LastUpdateStatus_Failed = LastUpdateStatus' "Failed"

pattern LastUpdateStatus_InProgress :: LastUpdateStatus
pattern LastUpdateStatus_InProgress = LastUpdateStatus' "InProgress"

pattern LastUpdateStatus_Successful :: LastUpdateStatus
pattern LastUpdateStatus_Successful = LastUpdateStatus' "Successful"

{-# COMPLETE
  LastUpdateStatus_Failed,
  LastUpdateStatus_InProgress,
  LastUpdateStatus_Successful,
  LastUpdateStatus'
  #-}
