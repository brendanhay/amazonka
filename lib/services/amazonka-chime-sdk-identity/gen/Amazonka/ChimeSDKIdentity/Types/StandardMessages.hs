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
-- Module      : Amazonka.ChimeSDKIdentity.Types.StandardMessages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.StandardMessages
  ( StandardMessages
      ( ..,
        StandardMessages_ALL,
        StandardMessages_AUTO,
        StandardMessages_MENTIONS,
        StandardMessages_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StandardMessages = StandardMessages'
  { fromStandardMessages ::
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

pattern StandardMessages_ALL :: StandardMessages
pattern StandardMessages_ALL = StandardMessages' "ALL"

pattern StandardMessages_AUTO :: StandardMessages
pattern StandardMessages_AUTO = StandardMessages' "AUTO"

pattern StandardMessages_MENTIONS :: StandardMessages
pattern StandardMessages_MENTIONS = StandardMessages' "MENTIONS"

pattern StandardMessages_NONE :: StandardMessages
pattern StandardMessages_NONE = StandardMessages' "NONE"

{-# COMPLETE
  StandardMessages_ALL,
  StandardMessages_AUTO,
  StandardMessages_MENTIONS,
  StandardMessages_NONE,
  StandardMessages'
  #-}
