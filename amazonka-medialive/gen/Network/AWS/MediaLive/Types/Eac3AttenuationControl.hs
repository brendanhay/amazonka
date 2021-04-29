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
-- Module      : Network.AWS.MediaLive.Types.Eac3AttenuationControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3AttenuationControl
  ( Eac3AttenuationControl
      ( ..,
        Eac3AttenuationControl_ATTENUATE_3_DB,
        Eac3AttenuationControl_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Eac3 Attenuation Control
newtype Eac3AttenuationControl = Eac3AttenuationControl'
  { fromEac3AttenuationControl ::
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

pattern Eac3AttenuationControl_ATTENUATE_3_DB :: Eac3AttenuationControl
pattern Eac3AttenuationControl_ATTENUATE_3_DB = Eac3AttenuationControl' "ATTENUATE_3_DB"

pattern Eac3AttenuationControl_NONE :: Eac3AttenuationControl
pattern Eac3AttenuationControl_NONE = Eac3AttenuationControl' "NONE"

{-# COMPLETE
  Eac3AttenuationControl_ATTENUATE_3_DB,
  Eac3AttenuationControl_NONE,
  Eac3AttenuationControl'
  #-}
