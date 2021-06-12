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
-- Module      : Network.AWS.CertificateManager.Types.RenewalEligibility
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalEligibility
  ( RenewalEligibility
      ( ..,
        RenewalEligibility_ELIGIBLE,
        RenewalEligibility_INELIGIBLE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RenewalEligibility = RenewalEligibility'
  { fromRenewalEligibility ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern RenewalEligibility_ELIGIBLE :: RenewalEligibility
pattern RenewalEligibility_ELIGIBLE = RenewalEligibility' "ELIGIBLE"

pattern RenewalEligibility_INELIGIBLE :: RenewalEligibility
pattern RenewalEligibility_INELIGIBLE = RenewalEligibility' "INELIGIBLE"

{-# COMPLETE
  RenewalEligibility_ELIGIBLE,
  RenewalEligibility_INELIGIBLE,
  RenewalEligibility'
  #-}
