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
-- Module      : Network.AWS.MediaLive.Types.H265FlickerAq
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FlickerAq
  ( H265FlickerAq
      ( ..,
        H265FlickerAq_DISABLED,
        H265FlickerAq_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | H265 Flicker Aq
newtype H265FlickerAq = H265FlickerAq'
  { fromH265FlickerAq ::
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

pattern H265FlickerAq_DISABLED :: H265FlickerAq
pattern H265FlickerAq_DISABLED = H265FlickerAq' "DISABLED"

pattern H265FlickerAq_ENABLED :: H265FlickerAq
pattern H265FlickerAq_ENABLED = H265FlickerAq' "ENABLED"

{-# COMPLETE
  H265FlickerAq_DISABLED,
  H265FlickerAq_ENABLED,
  H265FlickerAq'
  #-}
