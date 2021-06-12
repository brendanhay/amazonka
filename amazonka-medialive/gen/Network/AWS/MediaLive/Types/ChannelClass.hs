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
-- Module      : Network.AWS.MediaLive.Types.ChannelClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelClass
  ( ChannelClass
      ( ..,
        ChannelClass_SINGLE_PIPELINE,
        ChannelClass_STANDARD
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | A standard channel has two encoding pipelines and a single pipeline
-- channel only has one.
newtype ChannelClass = ChannelClass'
  { fromChannelClass ::
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

pattern ChannelClass_SINGLE_PIPELINE :: ChannelClass
pattern ChannelClass_SINGLE_PIPELINE = ChannelClass' "SINGLE_PIPELINE"

pattern ChannelClass_STANDARD :: ChannelClass
pattern ChannelClass_STANDARD = ChannelClass' "STANDARD"

{-# COMPLETE
  ChannelClass_SINGLE_PIPELINE,
  ChannelClass_STANDARD,
  ChannelClass'
  #-}
