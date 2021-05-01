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
-- Module      : Network.AWS.QLDB.Types.StreamStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.StreamStatus
  ( StreamStatus
      ( ..,
        StreamStatus_ACTIVE,
        StreamStatus_CANCELED,
        StreamStatus_COMPLETED,
        StreamStatus_FAILED,
        StreamStatus_IMPAIRED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StreamStatus = StreamStatus'
  { fromStreamStatus ::
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

pattern StreamStatus_ACTIVE :: StreamStatus
pattern StreamStatus_ACTIVE = StreamStatus' "ACTIVE"

pattern StreamStatus_CANCELED :: StreamStatus
pattern StreamStatus_CANCELED = StreamStatus' "CANCELED"

pattern StreamStatus_COMPLETED :: StreamStatus
pattern StreamStatus_COMPLETED = StreamStatus' "COMPLETED"

pattern StreamStatus_FAILED :: StreamStatus
pattern StreamStatus_FAILED = StreamStatus' "FAILED"

pattern StreamStatus_IMPAIRED :: StreamStatus
pattern StreamStatus_IMPAIRED = StreamStatus' "IMPAIRED"

{-# COMPLETE
  StreamStatus_ACTIVE,
  StreamStatus_CANCELED,
  StreamStatus_COMPLETED,
  StreamStatus_FAILED,
  StreamStatus_IMPAIRED,
  StreamStatus'
  #-}
