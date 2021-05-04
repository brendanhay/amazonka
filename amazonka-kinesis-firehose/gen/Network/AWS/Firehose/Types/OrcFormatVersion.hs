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
-- Module      : Network.AWS.Firehose.Types.OrcFormatVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcFormatVersion
  ( OrcFormatVersion
      ( ..,
        OrcFormatVersion_V0_11,
        OrcFormatVersion_V0_12
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OrcFormatVersion = OrcFormatVersion'
  { fromOrcFormatVersion ::
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

pattern OrcFormatVersion_V0_11 :: OrcFormatVersion
pattern OrcFormatVersion_V0_11 = OrcFormatVersion' "V0_11"

pattern OrcFormatVersion_V0_12 :: OrcFormatVersion
pattern OrcFormatVersion_V0_12 = OrcFormatVersion' "V0_12"

{-# COMPLETE
  OrcFormatVersion_V0_11,
  OrcFormatVersion_V0_12,
  OrcFormatVersion'
  #-}
