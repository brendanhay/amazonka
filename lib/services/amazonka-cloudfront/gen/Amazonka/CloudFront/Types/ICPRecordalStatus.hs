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
-- Module      : Amazonka.CloudFront.Types.ICPRecordalStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ICPRecordalStatus
  ( ICPRecordalStatus
      ( ..,
        ICPRecordalStatus_APPROVED,
        ICPRecordalStatus_PENDING,
        ICPRecordalStatus_SUSPENDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ICPRecordalStatus = ICPRecordalStatus'
  { fromICPRecordalStatus ::
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

pattern ICPRecordalStatus_APPROVED :: ICPRecordalStatus
pattern ICPRecordalStatus_APPROVED = ICPRecordalStatus' "APPROVED"

pattern ICPRecordalStatus_PENDING :: ICPRecordalStatus
pattern ICPRecordalStatus_PENDING = ICPRecordalStatus' "PENDING"

pattern ICPRecordalStatus_SUSPENDED :: ICPRecordalStatus
pattern ICPRecordalStatus_SUSPENDED = ICPRecordalStatus' "SUSPENDED"

{-# COMPLETE
  ICPRecordalStatus_APPROVED,
  ICPRecordalStatus_PENDING,
  ICPRecordalStatus_SUSPENDED,
  ICPRecordalStatus'
  #-}
