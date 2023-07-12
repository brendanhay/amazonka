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
-- Module      : Amazonka.GameLift.Types.GameServerGroupDeleteOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameServerGroupDeleteOption
  ( GameServerGroupDeleteOption
      ( ..,
        GameServerGroupDeleteOption_FORCE_DELETE,
        GameServerGroupDeleteOption_RETAIN,
        GameServerGroupDeleteOption_SAFE_DELETE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GameServerGroupDeleteOption = GameServerGroupDeleteOption'
  { fromGameServerGroupDeleteOption ::
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

pattern GameServerGroupDeleteOption_FORCE_DELETE :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOption_FORCE_DELETE = GameServerGroupDeleteOption' "FORCE_DELETE"

pattern GameServerGroupDeleteOption_RETAIN :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOption_RETAIN = GameServerGroupDeleteOption' "RETAIN"

pattern GameServerGroupDeleteOption_SAFE_DELETE :: GameServerGroupDeleteOption
pattern GameServerGroupDeleteOption_SAFE_DELETE = GameServerGroupDeleteOption' "SAFE_DELETE"

{-# COMPLETE
  GameServerGroupDeleteOption_FORCE_DELETE,
  GameServerGroupDeleteOption_RETAIN,
  GameServerGroupDeleteOption_SAFE_DELETE,
  GameServerGroupDeleteOption'
  #-}
