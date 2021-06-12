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
-- Module      : Network.AWS.SWF.Types.ChildPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildPolicy
  ( ChildPolicy
      ( ..,
        ChildPolicy_ABANDON,
        ChildPolicy_REQUEST_CANCEL,
        ChildPolicy_TERMINATE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ChildPolicy = ChildPolicy'
  { fromChildPolicy ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ChildPolicy_ABANDON :: ChildPolicy
pattern ChildPolicy_ABANDON = ChildPolicy' "ABANDON"

pattern ChildPolicy_REQUEST_CANCEL :: ChildPolicy
pattern ChildPolicy_REQUEST_CANCEL = ChildPolicy' "REQUEST_CANCEL"

pattern ChildPolicy_TERMINATE :: ChildPolicy
pattern ChildPolicy_TERMINATE = ChildPolicy' "TERMINATE"

{-# COMPLETE
  ChildPolicy_ABANDON,
  ChildPolicy_REQUEST_CANCEL,
  ChildPolicy_TERMINATE,
  ChildPolicy'
  #-}
