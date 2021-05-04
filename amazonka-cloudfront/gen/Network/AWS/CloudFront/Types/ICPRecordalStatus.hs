{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ICPRecordalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ICPRecordalStatus
  ( ICPRecordalStatus
      ( ..,
        ICPRecordalStatus_APPROVED,
        ICPRecordalStatus_PENDING,
        ICPRecordalStatus_SUSPENDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ICPRecordalStatus = ICPRecordalStatus'
  { fromICPRecordalStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
