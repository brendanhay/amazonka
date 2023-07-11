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
-- Module      : Amazonka.HoneyCode.Types.Format
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.Format
  ( Format
      ( ..,
        Format_ACCOUNTING,
        Format_AUTO,
        Format_CONTACT,
        Format_CURRENCY,
        Format_DATE,
        Format_DATE_TIME,
        Format_NUMBER,
        Format_PERCENTAGE,
        Format_ROWLINK,
        Format_ROWSET,
        Format_TEXT,
        Format_TIME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Format = Format' {fromFormat :: Data.Text}
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

pattern Format_ACCOUNTING :: Format
pattern Format_ACCOUNTING = Format' "ACCOUNTING"

pattern Format_AUTO :: Format
pattern Format_AUTO = Format' "AUTO"

pattern Format_CONTACT :: Format
pattern Format_CONTACT = Format' "CONTACT"

pattern Format_CURRENCY :: Format
pattern Format_CURRENCY = Format' "CURRENCY"

pattern Format_DATE :: Format
pattern Format_DATE = Format' "DATE"

pattern Format_DATE_TIME :: Format
pattern Format_DATE_TIME = Format' "DATE_TIME"

pattern Format_NUMBER :: Format
pattern Format_NUMBER = Format' "NUMBER"

pattern Format_PERCENTAGE :: Format
pattern Format_PERCENTAGE = Format' "PERCENTAGE"

pattern Format_ROWLINK :: Format
pattern Format_ROWLINK = Format' "ROWLINK"

pattern Format_ROWSET :: Format
pattern Format_ROWSET = Format' "ROWSET"

pattern Format_TEXT :: Format
pattern Format_TEXT = Format' "TEXT"

pattern Format_TIME :: Format
pattern Format_TIME = Format' "TIME"

{-# COMPLETE
  Format_ACCOUNTING,
  Format_AUTO,
  Format_CONTACT,
  Format_CURRENCY,
  Format_DATE,
  Format_DATE_TIME,
  Format_NUMBER,
  Format_PERCENTAGE,
  Format_ROWLINK,
  Format_ROWSET,
  Format_TEXT,
  Format_TIME,
  Format'
  #-}
