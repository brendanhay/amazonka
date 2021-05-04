{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.InputFormat
  ( InputFormat
      ( ..,
        InputFormat_ONE_DOC_PER_FILE,
        InputFormat_ONE_DOC_PER_LINE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InputFormat = InputFormat'
  { fromInputFormat ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern InputFormat_ONE_DOC_PER_FILE :: InputFormat
pattern InputFormat_ONE_DOC_PER_FILE = InputFormat' "ONE_DOC_PER_FILE"

pattern InputFormat_ONE_DOC_PER_LINE :: InputFormat
pattern InputFormat_ONE_DOC_PER_LINE = InputFormat' "ONE_DOC_PER_LINE"

{-# COMPLETE
  InputFormat_ONE_DOC_PER_FILE,
  InputFormat_ONE_DOC_PER_LINE,
  InputFormat'
  #-}
