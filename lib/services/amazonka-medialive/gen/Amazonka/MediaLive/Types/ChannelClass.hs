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
-- Module      : Amazonka.MediaLive.Types.ChannelClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ChannelClass
  ( ChannelClass
      ( ..,
        ChannelClass_SINGLE_PIPELINE,
        ChannelClass_STANDARD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A standard channel has two encoding pipelines and a single pipeline
-- channel only has one.
newtype ChannelClass = ChannelClass'
  { fromChannelClass ::
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

pattern ChannelClass_SINGLE_PIPELINE :: ChannelClass
pattern ChannelClass_SINGLE_PIPELINE = ChannelClass' "SINGLE_PIPELINE"

pattern ChannelClass_STANDARD :: ChannelClass
pattern ChannelClass_STANDARD = ChannelClass' "STANDARD"

{-# COMPLETE
  ChannelClass_SINGLE_PIPELINE,
  ChannelClass_STANDARD,
  ChannelClass'
  #-}
