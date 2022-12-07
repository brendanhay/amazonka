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
-- Module      : Amazonka.Kendra.Types.SlackEntity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SlackEntity
  ( SlackEntity
      ( ..,
        SlackEntity_DIRECT_MESSAGE,
        SlackEntity_GROUP_MESSAGE,
        SlackEntity_PRIVATE_CHANNEL,
        SlackEntity_PUBLIC_CHANNEL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SlackEntity = SlackEntity'
  { fromSlackEntity ::
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

pattern SlackEntity_DIRECT_MESSAGE :: SlackEntity
pattern SlackEntity_DIRECT_MESSAGE = SlackEntity' "DIRECT_MESSAGE"

pattern SlackEntity_GROUP_MESSAGE :: SlackEntity
pattern SlackEntity_GROUP_MESSAGE = SlackEntity' "GROUP_MESSAGE"

pattern SlackEntity_PRIVATE_CHANNEL :: SlackEntity
pattern SlackEntity_PRIVATE_CHANNEL = SlackEntity' "PRIVATE_CHANNEL"

pattern SlackEntity_PUBLIC_CHANNEL :: SlackEntity
pattern SlackEntity_PUBLIC_CHANNEL = SlackEntity' "PUBLIC_CHANNEL"

{-# COMPLETE
  SlackEntity_DIRECT_MESSAGE,
  SlackEntity_GROUP_MESSAGE,
  SlackEntity_PRIVATE_CHANNEL,
  SlackEntity_PUBLIC_CHANNEL,
  SlackEntity'
  #-}
