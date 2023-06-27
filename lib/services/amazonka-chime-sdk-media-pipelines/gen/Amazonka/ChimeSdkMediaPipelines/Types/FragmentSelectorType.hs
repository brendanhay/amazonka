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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.FragmentSelectorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.FragmentSelectorType
  ( FragmentSelectorType
      ( ..,
        FragmentSelectorType_ProducerTimestamp,
        FragmentSelectorType_ServerTimestamp
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FragmentSelectorType = FragmentSelectorType'
  { fromFragmentSelectorType ::
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

pattern FragmentSelectorType_ProducerTimestamp :: FragmentSelectorType
pattern FragmentSelectorType_ProducerTimestamp = FragmentSelectorType' "ProducerTimestamp"

pattern FragmentSelectorType_ServerTimestamp :: FragmentSelectorType
pattern FragmentSelectorType_ServerTimestamp = FragmentSelectorType' "ServerTimestamp"

{-# COMPLETE
  FragmentSelectorType_ProducerTimestamp,
  FragmentSelectorType_ServerTimestamp,
  FragmentSelectorType'
  #-}
