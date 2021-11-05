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
-- Module      : Amazonka.SES.Types.ReceiptFilterPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReceiptFilterPolicy
  ( ReceiptFilterPolicy
      ( ..,
        ReceiptFilterPolicy_Allow,
        ReceiptFilterPolicy_Block
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReceiptFilterPolicy = ReceiptFilterPolicy'
  { fromReceiptFilterPolicy ::
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

pattern ReceiptFilterPolicy_Allow :: ReceiptFilterPolicy
pattern ReceiptFilterPolicy_Allow = ReceiptFilterPolicy' "Allow"

pattern ReceiptFilterPolicy_Block :: ReceiptFilterPolicy
pattern ReceiptFilterPolicy_Block = ReceiptFilterPolicy' "Block"

{-# COMPLETE
  ReceiptFilterPolicy_Allow,
  ReceiptFilterPolicy_Block,
  ReceiptFilterPolicy'
  #-}
