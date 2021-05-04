{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3DrcProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3DrcProfile
  ( Ac3DrcProfile
      ( ..,
        Ac3DrcProfile_FILM_STANDARD,
        Ac3DrcProfile_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Ac3 Drc Profile
newtype Ac3DrcProfile = Ac3DrcProfile'
  { fromAc3DrcProfile ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
