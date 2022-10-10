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
-- Module      : Amazonka.CodeGuruReviewer.Types.ConfigFileState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.ConfigFileState
  ( ConfigFileState
      ( ..,
        ConfigFileState_Absent,
        ConfigFileState_Present,
        ConfigFileState_PresentWithErrors
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ConfigFileState = ConfigFileState'
  { fromConfigFileState ::
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

pattern ConfigFileState_Absent :: ConfigFileState
pattern ConfigFileState_Absent = ConfigFileState' "Absent"

pattern ConfigFileState_Present :: ConfigFileState
pattern ConfigFileState_Present = ConfigFileState' "Present"

pattern ConfigFileState_PresentWithErrors :: ConfigFileState
pattern ConfigFileState_PresentWithErrors = ConfigFileState' "PresentWithErrors"

{-# COMPLETE
  ConfigFileState_Absent,
  ConfigFileState_Present,
  ConfigFileState_PresentWithErrors,
  ConfigFileState'
  #-}
