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
-- Module      : Amazonka.MediaLive.Types.Ac3DrcProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Ac3DrcProfile
  ( Ac3DrcProfile
      ( ..,
        Ac3DrcProfile_FILM_STANDARD,
        Ac3DrcProfile_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Ac3 Drc Profile
newtype Ac3DrcProfile = Ac3DrcProfile'
  { fromAc3DrcProfile ::
      Core.Text
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

pattern Ac3DrcProfile_FILM_STANDARD :: Ac3DrcProfile
pattern Ac3DrcProfile_FILM_STANDARD = Ac3DrcProfile' "FILM_STANDARD"

pattern Ac3DrcProfile_NONE :: Ac3DrcProfile
pattern Ac3DrcProfile_NONE = Ac3DrcProfile' "NONE"

{-# COMPLETE
  Ac3DrcProfile_FILM_STANDARD,
  Ac3DrcProfile_NONE,
  Ac3DrcProfile'
  #-}
