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
-- Module      : Network.AWS.MediaConvert.Types.Eac3LfeControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3LfeControl
  ( Eac3LfeControl
      ( ..,
        Eac3LfeControl_LFE,
        Eac3LfeControl_NO_LFE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
newtype Eac3LfeControl = Eac3LfeControl'
  { fromEac3LfeControl ::
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

pattern Eac3LfeControl_LFE :: Eac3LfeControl
pattern Eac3LfeControl_LFE = Eac3LfeControl' "LFE"

pattern Eac3LfeControl_NO_LFE :: Eac3LfeControl
pattern Eac3LfeControl_NO_LFE = Eac3LfeControl' "NO_LFE"

{-# COMPLETE
  Eac3LfeControl_LFE,
  Eac3LfeControl_NO_LFE,
  Eac3LfeControl'
  #-}
