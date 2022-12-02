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
-- Module      : Amazonka.MediaLive.Types.EbuTtDDestinationStyleControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.EbuTtDDestinationStyleControl
  ( EbuTtDDestinationStyleControl
      ( ..,
        EbuTtDDestinationStyleControl_EXCLUDE,
        EbuTtDDestinationStyleControl_INCLUDE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ebu Tt DDestination Style Control
newtype EbuTtDDestinationStyleControl = EbuTtDDestinationStyleControl'
  { fromEbuTtDDestinationStyleControl ::
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

pattern EbuTtDDestinationStyleControl_EXCLUDE :: EbuTtDDestinationStyleControl
pattern EbuTtDDestinationStyleControl_EXCLUDE = EbuTtDDestinationStyleControl' "EXCLUDE"

pattern EbuTtDDestinationStyleControl_INCLUDE :: EbuTtDDestinationStyleControl
pattern EbuTtDDestinationStyleControl_INCLUDE = EbuTtDDestinationStyleControl' "INCLUDE"

{-# COMPLETE
  EbuTtDDestinationStyleControl_EXCLUDE,
  EbuTtDDestinationStyleControl_INCLUDE,
  EbuTtDDestinationStyleControl'
  #-}
