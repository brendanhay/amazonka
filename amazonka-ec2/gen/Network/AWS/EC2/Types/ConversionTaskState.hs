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
-- Module      : Network.AWS.EC2.Types.ConversionTaskState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTaskState
  ( ConversionTaskState
      ( ..,
        ConversionTaskState_Active,
        ConversionTaskState_Cancelled,
        ConversionTaskState_Cancelling,
        ConversionTaskState_Completed
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ConversionTaskState = ConversionTaskState'
  { fromConversionTaskState ::
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

pattern ConversionTaskState_Active :: ConversionTaskState
pattern ConversionTaskState_Active = ConversionTaskState' "active"

pattern ConversionTaskState_Cancelled :: ConversionTaskState
pattern ConversionTaskState_Cancelled = ConversionTaskState' "cancelled"

pattern ConversionTaskState_Cancelling :: ConversionTaskState
pattern ConversionTaskState_Cancelling = ConversionTaskState' "cancelling"

pattern ConversionTaskState_Completed :: ConversionTaskState
pattern ConversionTaskState_Completed = ConversionTaskState' "completed"

{-# COMPLETE
  ConversionTaskState_Active,
  ConversionTaskState_Cancelled,
  ConversionTaskState_Cancelling,
  ConversionTaskState_Completed,
  ConversionTaskState'
  #-}
