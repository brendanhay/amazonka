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
-- Module      : Network.AWS.MediaLive.Types.InputPreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputPreference
  ( InputPreference
      ( ..,
        InputPreference_EQUAL_INPUT_PREFERENCE,
        InputPreference_PRIMARY_INPUT_PREFERRED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Input preference when deciding which input to make active when a
-- previously failed input has recovered. If
-- \\\"EQUAL_INPUT_PREFERENCE\\\", then the active input will stay active
-- as long as it is healthy. If \\\"PRIMARY_INPUT_PREFERRED\\\", then
-- always switch back to the primary input when it is healthy.
newtype InputPreference = InputPreference'
  { fromInputPreference ::
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

pattern InputPreference_EQUAL_INPUT_PREFERENCE :: InputPreference
pattern InputPreference_EQUAL_INPUT_PREFERENCE = InputPreference' "EQUAL_INPUT_PREFERENCE"

pattern InputPreference_PRIMARY_INPUT_PREFERRED :: InputPreference
pattern InputPreference_PRIMARY_INPUT_PREFERRED = InputPreference' "PRIMARY_INPUT_PREFERRED"

{-# COMPLETE
  InputPreference_EQUAL_INPUT_PREFERENCE,
  InputPreference_PRIMARY_INPUT_PREFERRED,
  InputPreference'
  #-}
