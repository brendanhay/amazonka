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
-- Module      : Network.AWS.MediaLive.Types.BurnInAlignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInAlignment
  ( BurnInAlignment
      ( ..,
        BurnInAlignment_CENTERED,
        BurnInAlignment_LEFT,
        BurnInAlignment_SMART
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Burn In Alignment
newtype BurnInAlignment = BurnInAlignment'
  { fromBurnInAlignment ::
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

pattern BurnInAlignment_CENTERED :: BurnInAlignment
pattern BurnInAlignment_CENTERED = BurnInAlignment' "CENTERED"

pattern BurnInAlignment_LEFT :: BurnInAlignment
pattern BurnInAlignment_LEFT = BurnInAlignment' "LEFT"

pattern BurnInAlignment_SMART :: BurnInAlignment
pattern BurnInAlignment_SMART = BurnInAlignment' "SMART"

{-# COMPLETE
  BurnInAlignment_CENTERED,
  BurnInAlignment_LEFT,
  BurnInAlignment_SMART,
  BurnInAlignment'
  #-}
