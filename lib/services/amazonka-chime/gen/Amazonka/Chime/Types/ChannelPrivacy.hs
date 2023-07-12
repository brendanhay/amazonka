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
-- Module      : Amazonka.Chime.Types.ChannelPrivacy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelPrivacy
  ( ChannelPrivacy
      ( ..,
        ChannelPrivacy_PRIVATE,
        ChannelPrivacy_PUBLIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChannelPrivacy = ChannelPrivacy'
  { fromChannelPrivacy ::
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

pattern ChannelPrivacy_PRIVATE :: ChannelPrivacy
pattern ChannelPrivacy_PRIVATE = ChannelPrivacy' "PRIVATE"

pattern ChannelPrivacy_PUBLIC :: ChannelPrivacy
pattern ChannelPrivacy_PUBLIC = ChannelPrivacy' "PUBLIC"

{-# COMPLETE
  ChannelPrivacy_PRIVATE,
  ChannelPrivacy_PUBLIC,
  ChannelPrivacy'
  #-}
