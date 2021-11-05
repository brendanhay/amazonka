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
-- Module      : Amazonka.DataExchange.Types.Origin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.Origin
  ( Origin
      ( ..,
        Origin_ENTITLED,
        Origin_OWNED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers). When an owned
-- data set is published in a product, AWS Data Exchange creates a copy of
-- the data set. Subscribers can access that copy of the data set as an
-- entitled data set.
newtype Origin = Origin' {fromOrigin :: Core.Text}
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

pattern Origin_ENTITLED :: Origin
pattern Origin_ENTITLED = Origin' "ENTITLED"

pattern Origin_OWNED :: Origin
pattern Origin_OWNED = Origin' "OWNED"

{-# COMPLETE
  Origin_ENTITLED,
  Origin_OWNED,
  Origin'
  #-}
