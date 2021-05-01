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
-- Module      : Network.AWS.Kinesis.Types.ConsumerStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ConsumerStatus
  ( ConsumerStatus
      ( ..,
        ConsumerStatus_ACTIVE,
        ConsumerStatus_CREATING,
        ConsumerStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ConsumerStatus = ConsumerStatus'
  { fromConsumerStatus ::
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

pattern ConsumerStatus_ACTIVE :: ConsumerStatus
pattern ConsumerStatus_ACTIVE = ConsumerStatus' "ACTIVE"

pattern ConsumerStatus_CREATING :: ConsumerStatus
pattern ConsumerStatus_CREATING = ConsumerStatus' "CREATING"

pattern ConsumerStatus_DELETING :: ConsumerStatus
pattern ConsumerStatus_DELETING = ConsumerStatus' "DELETING"

{-# COMPLETE
  ConsumerStatus_ACTIVE,
  ConsumerStatus_CREATING,
  ConsumerStatus_DELETING,
  ConsumerStatus'
  #-}
