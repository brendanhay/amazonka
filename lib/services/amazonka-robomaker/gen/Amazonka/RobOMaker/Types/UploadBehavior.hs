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
-- Module      : Amazonka.RobOMaker.Types.UploadBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.UploadBehavior
  ( UploadBehavior
      ( ..,
        UploadBehavior_UPLOAD_ON_TERMINATE,
        UploadBehavior_UPLOAD_ROLLING_AUTO_REMOVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UploadBehavior = UploadBehavior'
  { fromUploadBehavior ::
      Data.Text
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

pattern UploadBehavior_UPLOAD_ON_TERMINATE :: UploadBehavior
pattern UploadBehavior_UPLOAD_ON_TERMINATE = UploadBehavior' "UPLOAD_ON_TERMINATE"

pattern UploadBehavior_UPLOAD_ROLLING_AUTO_REMOVE :: UploadBehavior
pattern UploadBehavior_UPLOAD_ROLLING_AUTO_REMOVE = UploadBehavior' "UPLOAD_ROLLING_AUTO_REMOVE"

{-# COMPLETE
  UploadBehavior_UPLOAD_ON_TERMINATE,
  UploadBehavior_UPLOAD_ROLLING_AUTO_REMOVE,
  UploadBehavior'
  #-}
