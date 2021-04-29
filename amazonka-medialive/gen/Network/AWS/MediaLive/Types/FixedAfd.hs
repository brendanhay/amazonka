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
-- Module      : Network.AWS.MediaLive.Types.FixedAfd
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FixedAfd
  ( FixedAfd
      ( ..,
        FixedAfd_AFD_0000,
        FixedAfd_AFD_0010,
        FixedAfd_AFD_0011,
        FixedAfd_AFD_0100,
        FixedAfd_AFD_1000,
        FixedAfd_AFD_1001,
        FixedAfd_AFD_1010,
        FixedAfd_AFD_1011,
        FixedAfd_AFD_1101,
        FixedAfd_AFD_1110,
        FixedAfd_AFD_1111
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Fixed Afd
newtype FixedAfd = FixedAfd'
  { fromFixedAfd ::
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

pattern FixedAfd_AFD_0000 :: FixedAfd
pattern FixedAfd_AFD_0000 = FixedAfd' "AFD_0000"

pattern FixedAfd_AFD_0010 :: FixedAfd
pattern FixedAfd_AFD_0010 = FixedAfd' "AFD_0010"

pattern FixedAfd_AFD_0011 :: FixedAfd
pattern FixedAfd_AFD_0011 = FixedAfd' "AFD_0011"

pattern FixedAfd_AFD_0100 :: FixedAfd
pattern FixedAfd_AFD_0100 = FixedAfd' "AFD_0100"

pattern FixedAfd_AFD_1000 :: FixedAfd
pattern FixedAfd_AFD_1000 = FixedAfd' "AFD_1000"

pattern FixedAfd_AFD_1001 :: FixedAfd
pattern FixedAfd_AFD_1001 = FixedAfd' "AFD_1001"

pattern FixedAfd_AFD_1010 :: FixedAfd
pattern FixedAfd_AFD_1010 = FixedAfd' "AFD_1010"

pattern FixedAfd_AFD_1011 :: FixedAfd
pattern FixedAfd_AFD_1011 = FixedAfd' "AFD_1011"

pattern FixedAfd_AFD_1101 :: FixedAfd
pattern FixedAfd_AFD_1101 = FixedAfd' "AFD_1101"

pattern FixedAfd_AFD_1110 :: FixedAfd
pattern FixedAfd_AFD_1110 = FixedAfd' "AFD_1110"

pattern FixedAfd_AFD_1111 :: FixedAfd
pattern FixedAfd_AFD_1111 = FixedAfd' "AFD_1111"

{-# COMPLETE
  FixedAfd_AFD_0000,
  FixedAfd_AFD_0010,
  FixedAfd_AFD_0011,
  FixedAfd_AFD_0100,
  FixedAfd_AFD_1000,
  FixedAfd_AFD_1001,
  FixedAfd_AFD_1010,
  FixedAfd_AFD_1011,
  FixedAfd_AFD_1101,
  FixedAfd_AFD_1110,
  FixedAfd_AFD_1111,
  FixedAfd'
  #-}
