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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ContentShareLayoutOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ContentShareLayoutOption
  ( ContentShareLayoutOption
      ( ..,
        ContentShareLayoutOption_Horizontal,
        ContentShareLayoutOption_PresenterOnly,
        ContentShareLayoutOption_Vertical
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContentShareLayoutOption = ContentShareLayoutOption'
  { fromContentShareLayoutOption ::
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

pattern ContentShareLayoutOption_Horizontal :: ContentShareLayoutOption
pattern ContentShareLayoutOption_Horizontal = ContentShareLayoutOption' "Horizontal"

pattern ContentShareLayoutOption_PresenterOnly :: ContentShareLayoutOption
pattern ContentShareLayoutOption_PresenterOnly = ContentShareLayoutOption' "PresenterOnly"

pattern ContentShareLayoutOption_Vertical :: ContentShareLayoutOption
pattern ContentShareLayoutOption_Vertical = ContentShareLayoutOption' "Vertical"

{-# COMPLETE
  ContentShareLayoutOption_Horizontal,
  ContentShareLayoutOption_PresenterOnly,
  ContentShareLayoutOption_Vertical,
  ContentShareLayoutOption'
  #-}
