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
-- Module      : Network.AWS.MediaConvert.Types.Xavc4kIntraVbrProfileClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Xavc4kIntraVbrProfileClass
  ( Xavc4kIntraVbrProfileClass
      ( ..,
        Xavc4kIntraVbrProfileClass_CLASS_100,
        Xavc4kIntraVbrProfileClass_CLASS_300,
        Xavc4kIntraVbrProfileClass_CLASS_480
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the XAVC Intra 4k (VBR) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
newtype Xavc4kIntraVbrProfileClass = Xavc4kIntraVbrProfileClass'
  { fromXavc4kIntraVbrProfileClass ::
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
