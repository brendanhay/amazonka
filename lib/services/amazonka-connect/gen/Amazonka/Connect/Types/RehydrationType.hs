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
-- Module      : Amazonka.Connect.Types.RehydrationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RehydrationType
  ( RehydrationType
      ( ..,
        RehydrationType_ENTIRE_PAST_SESSION,
        RehydrationType_FROM_SEGMENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RehydrationType = RehydrationType'
  { fromRehydrationType ::
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

pattern RehydrationType_ENTIRE_PAST_SESSION :: RehydrationType
pattern RehydrationType_ENTIRE_PAST_SESSION = RehydrationType' "ENTIRE_PAST_SESSION"

pattern RehydrationType_FROM_SEGMENT :: RehydrationType
pattern RehydrationType_FROM_SEGMENT = RehydrationType' "FROM_SEGMENT"

{-# COMPLETE
  RehydrationType_ENTIRE_PAST_SESSION,
  RehydrationType_FROM_SEGMENT,
  RehydrationType'
  #-}
