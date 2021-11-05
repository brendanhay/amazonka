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
-- Module      : Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileClass
  ( XavcHdIntraCbgProfileClass
      ( ..,
        XavcHdIntraCbgProfileClass_CLASS_100,
        XavcHdIntraCbgProfileClass_CLASS_200,
        XavcHdIntraCbgProfileClass_CLASS_50
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Specify the XAVC Intra HD (CBG) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
newtype XavcHdIntraCbgProfileClass = XavcHdIntraCbgProfileClass'
  { fromXavcHdIntraCbgProfileClass ::
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

pattern XavcHdIntraCbgProfileClass_CLASS_100 :: XavcHdIntraCbgProfileClass
pattern XavcHdIntraCbgProfileClass_CLASS_100 = XavcHdIntraCbgProfileClass' "CLASS_100"

pattern XavcHdIntraCbgProfileClass_CLASS_200 :: XavcHdIntraCbgProfileClass
pattern XavcHdIntraCbgProfileClass_CLASS_200 = XavcHdIntraCbgProfileClass' "CLASS_200"

pattern XavcHdIntraCbgProfileClass_CLASS_50 :: XavcHdIntraCbgProfileClass
pattern XavcHdIntraCbgProfileClass_CLASS_50 = XavcHdIntraCbgProfileClass' "CLASS_50"

{-# COMPLETE
  XavcHdIntraCbgProfileClass_CLASS_100,
  XavcHdIntraCbgProfileClass_CLASS_200,
  XavcHdIntraCbgProfileClass_CLASS_50,
  XavcHdIntraCbgProfileClass'
  #-}
