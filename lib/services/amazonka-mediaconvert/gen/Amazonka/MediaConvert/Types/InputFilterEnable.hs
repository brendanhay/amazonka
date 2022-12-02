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
-- Module      : Amazonka.MediaConvert.Types.InputFilterEnable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.InputFilterEnable
  ( InputFilterEnable
      ( ..,
        InputFilterEnable_AUTO,
        InputFilterEnable_DISABLE,
        InputFilterEnable_FORCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how the transcoding service applies the denoise and deblock
-- filters. You must also enable the filters separately, with Denoise
-- (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The
-- transcoding service determines whether to apply filtering, depending on
-- input type and quality. * Disable - The input is not filtered. This is
-- true even if you use the API to enable them in (InputDeblockFilter) and
-- (InputDeblockFilter). * Force - The input is filtered regardless of
-- input type.
newtype InputFilterEnable = InputFilterEnable'
  { fromInputFilterEnable ::
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

pattern InputFilterEnable_AUTO :: InputFilterEnable
pattern InputFilterEnable_AUTO = InputFilterEnable' "AUTO"

pattern InputFilterEnable_DISABLE :: InputFilterEnable
pattern InputFilterEnable_DISABLE = InputFilterEnable' "DISABLE"

pattern InputFilterEnable_FORCE :: InputFilterEnable
pattern InputFilterEnable_FORCE = InputFilterEnable' "FORCE"

{-# COMPLETE
  InputFilterEnable_AUTO,
  InputFilterEnable_DISABLE,
  InputFilterEnable_FORCE,
  InputFilterEnable'
  #-}
