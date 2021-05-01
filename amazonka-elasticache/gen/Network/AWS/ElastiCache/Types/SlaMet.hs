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
-- Module      : Network.AWS.ElastiCache.Types.SlaMet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SlaMet
  ( SlaMet
      ( ..,
        SlaMet_N_a,
        SlaMet_No,
        SlaMet_Yes
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SlaMet = SlaMet' {fromSlaMet :: Prelude.Text}
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

pattern SlaMet_N_a :: SlaMet
pattern SlaMet_N_a = SlaMet' "n/a"

pattern SlaMet_No :: SlaMet
pattern SlaMet_No = SlaMet' "no"

pattern SlaMet_Yes :: SlaMet
pattern SlaMet_Yes = SlaMet' "yes"

{-# COMPLETE
  SlaMet_N_a,
  SlaMet_No,
  SlaMet_Yes,
  SlaMet'
  #-}
