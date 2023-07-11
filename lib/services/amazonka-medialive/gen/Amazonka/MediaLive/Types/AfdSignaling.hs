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
-- Module      : Amazonka.MediaLive.Types.AfdSignaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AfdSignaling
  ( AfdSignaling
      ( ..,
        AfdSignaling_AUTO,
        AfdSignaling_FIXED,
        AfdSignaling_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Afd Signaling
newtype AfdSignaling = AfdSignaling'
  { fromAfdSignaling ::
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

pattern AfdSignaling_AUTO :: AfdSignaling
pattern AfdSignaling_AUTO = AfdSignaling' "AUTO"

pattern AfdSignaling_FIXED :: AfdSignaling
pattern AfdSignaling_FIXED = AfdSignaling' "FIXED"

pattern AfdSignaling_NONE :: AfdSignaling
pattern AfdSignaling_NONE = AfdSignaling' "NONE"

{-# COMPLETE
  AfdSignaling_AUTO,
  AfdSignaling_FIXED,
  AfdSignaling_NONE,
  AfdSignaling'
  #-}
