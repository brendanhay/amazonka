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
-- Module      : Network.AWS.MediaLive.Types.AfdSignaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AfdSignaling
  ( AfdSignaling
      ( ..,
        AfdSignaling_AUTO,
        AfdSignaling_FIXED,
        AfdSignaling_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Afd Signaling
newtype AfdSignaling = AfdSignaling'
  { fromAfdSignaling ::
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
