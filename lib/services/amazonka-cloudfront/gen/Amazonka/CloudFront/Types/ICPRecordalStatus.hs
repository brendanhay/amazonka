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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype ICPRecordalStatus = ICPRecordalStatus'
  { fromICPRecordalStatus ::
      Core.Text
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
