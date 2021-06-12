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
-- Module      : Network.AWS.Lambda.Types.InvocationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.InvocationType
  ( InvocationType
      ( ..,
        InvocationType_DryRun,
        InvocationType_Event,
        InvocationType_RequestResponse
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InvocationType = InvocationType'
  { fromInvocationType ::
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

pattern InvocationType_DryRun :: InvocationType
pattern InvocationType_DryRun = InvocationType' "DryRun"

pattern InvocationType_Event :: InvocationType
pattern InvocationType_Event = InvocationType' "Event"

pattern InvocationType_RequestResponse :: InvocationType
pattern InvocationType_RequestResponse = InvocationType' "RequestResponse"

{-# COMPLETE
  InvocationType_DryRun,
  InvocationType_Event,
  InvocationType_RequestResponse,
  InvocationType'
  #-}
