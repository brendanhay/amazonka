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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputStartingPosition
  ( InputStartingPosition
      ( ..,
        InputStartingPosition_LAST_STOPPED_POINT,
        InputStartingPosition_NOW,
        InputStartingPosition_TRIM_HORIZON
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InputStartingPosition = InputStartingPosition'
  { fromInputStartingPosition ::
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

pattern InputStartingPosition_LAST_STOPPED_POINT :: InputStartingPosition
pattern InputStartingPosition_LAST_STOPPED_POINT = InputStartingPosition' "LAST_STOPPED_POINT"

pattern InputStartingPosition_NOW :: InputStartingPosition
pattern InputStartingPosition_NOW = InputStartingPosition' "NOW"

pattern InputStartingPosition_TRIM_HORIZON :: InputStartingPosition
pattern InputStartingPosition_TRIM_HORIZON = InputStartingPosition' "TRIM_HORIZON"

{-# COMPLETE
  InputStartingPosition_LAST_STOPPED_POINT,
  InputStartingPosition_NOW,
  InputStartingPosition_TRIM_HORIZON,
  InputStartingPosition'
  #-}
