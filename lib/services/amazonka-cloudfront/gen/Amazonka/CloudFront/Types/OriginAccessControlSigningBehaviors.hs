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
-- Module      : Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
  ( OriginAccessControlSigningBehaviors
      ( ..,
        OriginAccessControlSigningBehaviors_Always,
        OriginAccessControlSigningBehaviors_Never,
        OriginAccessControlSigningBehaviors_No_override
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OriginAccessControlSigningBehaviors = OriginAccessControlSigningBehaviors'
  { fromOriginAccessControlSigningBehaviors ::
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

pattern OriginAccessControlSigningBehaviors_Always :: OriginAccessControlSigningBehaviors
pattern OriginAccessControlSigningBehaviors_Always = OriginAccessControlSigningBehaviors' "always"

pattern OriginAccessControlSigningBehaviors_Never :: OriginAccessControlSigningBehaviors
pattern OriginAccessControlSigningBehaviors_Never = OriginAccessControlSigningBehaviors' "never"

pattern OriginAccessControlSigningBehaviors_No_override :: OriginAccessControlSigningBehaviors
pattern OriginAccessControlSigningBehaviors_No_override = OriginAccessControlSigningBehaviors' "no-override"

{-# COMPLETE
  OriginAccessControlSigningBehaviors_Always,
  OriginAccessControlSigningBehaviors_Never,
  OriginAccessControlSigningBehaviors_No_override,
  OriginAccessControlSigningBehaviors'
  #-}
