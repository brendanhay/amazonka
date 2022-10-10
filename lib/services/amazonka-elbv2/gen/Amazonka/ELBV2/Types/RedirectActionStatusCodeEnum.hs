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
-- Module      : Amazonka.ELBV2.Types.RedirectActionStatusCodeEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.RedirectActionStatusCodeEnum
  ( RedirectActionStatusCodeEnum
      ( ..,
        RedirectActionStatusCodeEnum_HTTP_301,
        RedirectActionStatusCodeEnum_HTTP_302
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RedirectActionStatusCodeEnum = RedirectActionStatusCodeEnum'
  { fromRedirectActionStatusCodeEnum ::
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

pattern RedirectActionStatusCodeEnum_HTTP_301 :: RedirectActionStatusCodeEnum
pattern RedirectActionStatusCodeEnum_HTTP_301 = RedirectActionStatusCodeEnum' "HTTP_301"

pattern RedirectActionStatusCodeEnum_HTTP_302 :: RedirectActionStatusCodeEnum
pattern RedirectActionStatusCodeEnum_HTTP_302 = RedirectActionStatusCodeEnum' "HTTP_302"

{-# COMPLETE
  RedirectActionStatusCodeEnum_HTTP_301,
  RedirectActionStatusCodeEnum_HTTP_302,
  RedirectActionStatusCodeEnum'
  #-}
