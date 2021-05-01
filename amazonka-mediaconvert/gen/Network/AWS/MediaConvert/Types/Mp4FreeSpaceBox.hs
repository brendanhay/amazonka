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
-- Module      : Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
  ( Mp4FreeSpaceBox
      ( ..,
        Mp4FreeSpaceBox_EXCLUDE,
        Mp4FreeSpaceBox_INCLUDE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Inserts a free-space box immediately after the moov box.
newtype Mp4FreeSpaceBox = Mp4FreeSpaceBox'
  { fromMp4FreeSpaceBox ::
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

pattern Mp4FreeSpaceBox_EXCLUDE :: Mp4FreeSpaceBox
pattern Mp4FreeSpaceBox_EXCLUDE = Mp4FreeSpaceBox' "EXCLUDE"

pattern Mp4FreeSpaceBox_INCLUDE :: Mp4FreeSpaceBox
pattern Mp4FreeSpaceBox_INCLUDE = Mp4FreeSpaceBox' "INCLUDE"

{-# COMPLETE
  Mp4FreeSpaceBox_EXCLUDE,
  Mp4FreeSpaceBox_INCLUDE,
  Mp4FreeSpaceBox'
  #-}
