-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalEligibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RenewalEligibility
  ( RenewalEligibility
      ( RenewalEligibility',
        Eligible,
        Ineligible
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RenewalEligibility = RenewalEligibility' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Eligible :: RenewalEligibility
pattern Eligible = RenewalEligibility' "ELIGIBLE"

pattern Ineligible :: RenewalEligibility
pattern Ineligible = RenewalEligibility' "INELIGIBLE"

{-# COMPLETE
  Eligible,
  Ineligible,
  RenewalEligibility'
  #-}
