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
-- Module      : Amazonka.QuickSight.Types.BaseMapStyleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BaseMapStyleType
  ( BaseMapStyleType
      ( ..,
        BaseMapStyleType_DARK_GRAY,
        BaseMapStyleType_IMAGERY,
        BaseMapStyleType_LIGHT_GRAY,
        BaseMapStyleType_STREET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BaseMapStyleType = BaseMapStyleType'
  { fromBaseMapStyleType ::
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

pattern BaseMapStyleType_DARK_GRAY :: BaseMapStyleType
pattern BaseMapStyleType_DARK_GRAY = BaseMapStyleType' "DARK_GRAY"

pattern BaseMapStyleType_IMAGERY :: BaseMapStyleType
pattern BaseMapStyleType_IMAGERY = BaseMapStyleType' "IMAGERY"

pattern BaseMapStyleType_LIGHT_GRAY :: BaseMapStyleType
pattern BaseMapStyleType_LIGHT_GRAY = BaseMapStyleType' "LIGHT_GRAY"

pattern BaseMapStyleType_STREET :: BaseMapStyleType
pattern BaseMapStyleType_STREET = BaseMapStyleType' "STREET"

{-# COMPLETE
  BaseMapStyleType_DARK_GRAY,
  BaseMapStyleType_IMAGERY,
  BaseMapStyleType_LIGHT_GRAY,
  BaseMapStyleType_STREET,
  BaseMapStyleType'
  #-}
