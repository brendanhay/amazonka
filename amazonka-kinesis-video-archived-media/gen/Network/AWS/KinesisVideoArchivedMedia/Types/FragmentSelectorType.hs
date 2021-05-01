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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
  ( FragmentSelectorType
      ( ..,
        FragmentSelectorType_PRODUCER_TIMESTAMP,
        FragmentSelectorType_SERVER_TIMESTAMP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype FragmentSelectorType = FragmentSelectorType'
  { fromFragmentSelectorType ::
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

pattern FragmentSelectorType_PRODUCER_TIMESTAMP :: FragmentSelectorType
pattern FragmentSelectorType_PRODUCER_TIMESTAMP = FragmentSelectorType' "PRODUCER_TIMESTAMP"

pattern FragmentSelectorType_SERVER_TIMESTAMP :: FragmentSelectorType
pattern FragmentSelectorType_SERVER_TIMESTAMP = FragmentSelectorType' "SERVER_TIMESTAMP"

{-# COMPLETE
  FragmentSelectorType_PRODUCER_TIMESTAMP,
  FragmentSelectorType_SERVER_TIMESTAMP,
  FragmentSelectorType'
  #-}
