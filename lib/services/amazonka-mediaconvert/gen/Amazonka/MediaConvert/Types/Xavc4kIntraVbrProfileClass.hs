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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileClass
  ( Xavc4kIntraVbrProfileClass
      ( ..,
        Xavc4kIntraVbrProfileClass_CLASS_100,
        Xavc4kIntraVbrProfileClass_CLASS_300,
        Xavc4kIntraVbrProfileClass_CLASS_480
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the XAVC Intra 4k (VBR) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
newtype Xavc4kIntraVbrProfileClass = Xavc4kIntraVbrProfileClass'
  { fromXavc4kIntraVbrProfileClass ::
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

pattern Xavc4kIntraVbrProfileClass_CLASS_100 :: Xavc4kIntraVbrProfileClass
pattern Xavc4kIntraVbrProfileClass_CLASS_100 = Xavc4kIntraVbrProfileClass' "CLASS_100"

pattern Xavc4kIntraVbrProfileClass_CLASS_300 :: Xavc4kIntraVbrProfileClass
pattern Xavc4kIntraVbrProfileClass_CLASS_300 = Xavc4kIntraVbrProfileClass' "CLASS_300"

pattern Xavc4kIntraVbrProfileClass_CLASS_480 :: Xavc4kIntraVbrProfileClass
pattern Xavc4kIntraVbrProfileClass_CLASS_480 = Xavc4kIntraVbrProfileClass' "CLASS_480"

{-# COMPLETE
  Xavc4kIntraVbrProfileClass_CLASS_100,
  Xavc4kIntraVbrProfileClass_CLASS_300,
  Xavc4kIntraVbrProfileClass_CLASS_480,
  Xavc4kIntraVbrProfileClass'
  #-}
