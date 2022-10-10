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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormButtonsPosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormButtonsPosition
  ( FormButtonsPosition
      ( ..,
        FormButtonsPosition_Bottom,
        FormButtonsPosition_Top,
        FormButtonsPosition_Top_and_bottom
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FormButtonsPosition = FormButtonsPosition'
  { fromFormButtonsPosition ::
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

pattern FormButtonsPosition_Bottom :: FormButtonsPosition
pattern FormButtonsPosition_Bottom = FormButtonsPosition' "bottom"

pattern FormButtonsPosition_Top :: FormButtonsPosition
pattern FormButtonsPosition_Top = FormButtonsPosition' "top"

pattern FormButtonsPosition_Top_and_bottom :: FormButtonsPosition
pattern FormButtonsPosition_Top_and_bottom = FormButtonsPosition' "top_and_bottom"

{-# COMPLETE
  FormButtonsPosition_Bottom,
  FormButtonsPosition_Top,
  FormButtonsPosition_Top_and_bottom,
  FormButtonsPosition'
  #-}
