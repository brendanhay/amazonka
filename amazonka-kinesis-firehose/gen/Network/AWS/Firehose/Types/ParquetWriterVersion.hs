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
-- Module      : Network.AWS.Firehose.Types.ParquetWriterVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetWriterVersion
  ( ParquetWriterVersion
      ( ..,
        ParquetWriterVersion_V1,
        ParquetWriterVersion_V2
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ParquetWriterVersion = ParquetWriterVersion'
  { fromParquetWriterVersion ::
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

pattern ParquetWriterVersion_V1 :: ParquetWriterVersion
pattern ParquetWriterVersion_V1 = ParquetWriterVersion' "V1"

pattern ParquetWriterVersion_V2 :: ParquetWriterVersion
pattern ParquetWriterVersion_V2 = ParquetWriterVersion' "V2"

{-# COMPLETE
  ParquetWriterVersion_V1,
  ParquetWriterVersion_V2,
  ParquetWriterVersion'
  #-}
